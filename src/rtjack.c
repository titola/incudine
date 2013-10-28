/*
 * Copyright (c) 2013 Tito Latini
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <jack/jack.h>
#include "common.h"

#define __JA_RUNNING     (0)
#define __JA_STOPPED     (1)
#define __JA_SHUTDOWN    (2)

#define SBCL_SIG_STOP_FOR_GC  (SIGUSR2)

static jack_client_t *client = NULL;
static SAMPLE ja_sample_rate;
static unsigned int ja_in_channels, ja_out_channels, ja_frames;
static size_t ja_buffer_bytes;
static int ja_status = __JA_STOPPED;
static int ja_lisp_busy;
static jack_default_audio_sample_t **ja_inputs, **ja_outputs;
static jack_port_t **input_ports, **output_ports;
static char **input_port_names, **output_port_names;
static pthread_mutex_t ja_lisp_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  ja_lisp_cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t ja_c_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  ja_c_cond = PTHREAD_COND_INITIALIZER;
static char ja_error_msg[256];
static sigset_t sig_stop_for_gc;

int ja_stop();

static void ja_shutdown(void *arg);

static void ja_terminate(void *arg);

static void ja_set_error_msg(const char *msg)
{
    strncpy(ja_error_msg, msg, 256);
}

char *ja_get_error_msg()
{
    return ja_error_msg;
}

#define ja_free(VAR)      \
    if (VAR != NULL) {    \
        free(VAR);        \
        VAR = NULL;       \
    }

static void ja_error(const char *msg) {
    ja_set_error_msg(msg);
    ja_stop(NULL);
}

#define ja_check_ptr(VAR)           \
    if (VAR == NULL) {              \
        ja_error("malloc failure"); \
        return 1;                   \
    }

jack_nframes_t ja_cycle_begin ()
{
    int i;
    jack_nframes_t frames;

    if (ja_status != __JA_RUNNING)
        return 0;
    /*
     * We are calling `ja_cycle_begin' from lisp, and the signal
     * sent by SBCL during the gc (SIGUSR2) interrupts `sem_timedwait'
     * used by Jack, therefore we have to temporarily block this signal.
     * The gc is inhibited because we are inside SB-SYS:WITHOUT-GCING.
     */
    pthread_sigmask(SIG_BLOCK, &sig_stop_for_gc, NULL);
    frames = jack_cycle_wait(client);

    if (ja_status != __JA_RUNNING) {
        pthread_sigmask(SIG_UNBLOCK, &sig_stop_for_gc, NULL);
        return 0;
    }
    for (i=0; i<ja_in_channels; i++)
        ja_inputs[i] = jack_port_get_buffer(input_ports[i], frames);

    for (i=0; i<ja_out_channels; i++)
        ja_outputs[i] = jack_port_get_buffer(output_ports[i], frames);

    pthread_sigmask(SIG_UNBLOCK, &sig_stop_for_gc, NULL);
    return frames;
}

void ja_cycle_signal(int status)
{
    if (status != __JA_RUNNING)
        ja_status = status;
    jack_cycle_signal(client, status);
}

/* Lisp rt thread is busy ? */
void ja_set_lisp_busy_state(int status)
{
    ja_lisp_busy = status;
}

#define __ja_condition_wait(COND,LOCK)  \
    pthread_mutex_lock(LOCK);           \
    pthread_cond_wait(COND, LOCK);      \
    pthread_mutex_unlock(LOCK);

#define __ja_condition_signal(COND,LOCK)  \
    pthread_mutex_lock(LOCK);             \
    pthread_cond_signal(COND);            \
    pthread_mutex_unlock(LOCK);

/* Wait on the lisp realtime thread */
void ja_condition_wait()
{
    __ja_condition_wait(&ja_lisp_cond, &ja_lisp_lock);
}

/* Transfer the control of the client to C realtime thread */
void ja_transfer_to_c_thread()
{
    ja_lisp_busy = 1;
    __ja_condition_signal(&ja_c_cond, &ja_c_lock);
}

static void* ja_thread(void *arg)
{
    (void) arg;

    while(1) {
        /* You say goodbye, I say hello */
        if (ja_status != __JA_RUNNING)
            return 0;

        if (ja_lisp_busy != 0) {
            int i;
            jack_nframes_t frames = jack_cycle_wait(client);

            if (ja_frames != frames) {
                /* Buffer size is changed */
                ja_frames = frames;
                ja_buffer_bytes = frames*sizeof(jack_default_audio_sample_t);
            }

            for (i=0; i<ja_out_channels; i++)
                ja_outputs[i] = jack_port_get_buffer(output_ports[i], ja_frames);
            /* Silence please, lisp is busy */
            for (i=0; i<ja_out_channels; i++)
                memset(ja_outputs[i], 0, ja_buffer_bytes);

            jack_cycle_signal(client, ja_status);
        } else {
            /*
             * Transfer the control of the client to lisp realtime thread
             * and block the current thread.
             * Notice it is called ONLY ONE TIME after the first cycle
             * and ONLY ONE TIME after the gc in SBCL. The rt lisp thread
             * uses `jack_cycle_wait' and `jack_cycle_signal' with the
             * actual jack client. Practically, this thread is an emergency
             * exit when we use an implementation of Common Lisp with a gc
             * which stops the rt lisp thread. If the implementation of CL
             * has a realtime gc, there aren't other transfers of the control
             * from C to Lisp and vice versa.
             */
            __ja_condition_signal(&ja_lisp_cond, &ja_lisp_lock);
            __ja_condition_wait(&ja_c_cond, &ja_c_lock);
        }
    }
    return 0;
}

int ja_get_buffer_size()
{
    return ja_frames;
}

SAMPLE ja_get_sample_rate()
{
    return ja_sample_rate;
}

static int ja_register_ports()
{
    int i;

    input_ports = (jack_port_t**) malloc(sizeof(jack_port_t*)*ja_in_channels);
    ja_check_ptr(input_ports);
    input_port_names = (char**) malloc(sizeof(char*)*ja_in_channels);
    ja_check_ptr(input_port_names);

    for (i=0; i<ja_in_channels; i++) {
        input_port_names[i] = (char*) malloc(sizeof(char)*16);
        ja_check_ptr(input_port_names[i]);
        sprintf(input_port_names[i], "in_%d", i+1);
        input_ports[i] = jack_port_register (client, input_port_names[i],
                                             JACK_DEFAULT_AUDIO_TYPE,
                                             JackPortIsInput, 0);
    }

    output_ports = (jack_port_t**) malloc(sizeof(jack_port_t*)*ja_out_channels);
    ja_check_ptr(output_ports);
    output_port_names = (char**) malloc(sizeof(char*)*ja_out_channels);
    ja_check_ptr(output_port_names);

    for (i=0; i<ja_out_channels; i++) {
        output_port_names[i] = (char*) malloc(sizeof(char)*16);
        ja_check_ptr(output_port_names[i]);
        sprintf(output_port_names[i], "out_%d", i+1);
        output_ports[i] = jack_port_register (client, output_port_names[i],
                                              JACK_DEFAULT_AUDIO_TYPE,
                                              JackPortIsOutput, 0);
    }
    return 0;
}

static int ja_connect_client()
{
    char **ports;
    int i;

    ports = (char**) jack_get_ports(client, NULL, NULL,
                                    JackPortIsPhysical|JackPortIsOutput);
    if (ports == NULL)
        return 1;
    for (i=0; i<ja_in_channels && ports[i] != NULL; i++)
        jack_connect(client, ports[i], jack_port_name(input_ports[i]));
    jack_free(ports);

    ports = (char**) jack_get_ports(client, NULL, NULL,
                                    JackPortIsPhysical|JackPortIsInput);
    if (ports == NULL)
        return 1;
    for (i=0; i<ja_out_channels && ports[i] != NULL; i++)
        jack_connect(client, jack_port_name(output_ports[i]), ports[i]);
    jack_free(ports);

    return 0;
}

int ja_initialize(SAMPLE srate, unsigned int input_channels,
                  unsigned int output_channels, unsigned int nframes,
                  const char* client_name)
{
    sigset_t sset;
    (void) srate;
    (void) nframes;

    ja_error_msg[0] = '\0';
    client = jack_client_open(client_name, JackNullOption, NULL);
    if (client == NULL) {
        ja_set_error_msg("jack_client_open failure");
        return 1;
    }
    ja_frames = jack_get_buffer_size(client);
    ja_sample_rate = (SAMPLE) jack_get_sample_rate(client);
    ja_in_channels = input_channels;
    ja_out_channels = output_channels;
    ja_buffer_bytes = ja_frames*sizeof(jack_default_audio_sample_t);

    ja_inputs = (jack_default_audio_sample_t**)
                   malloc(sizeof(jack_default_audio_sample_t*)*input_channels);
    ja_check_ptr(ja_inputs);

    ja_outputs = (jack_default_audio_sample_t**)
                    malloc(sizeof(jack_default_audio_sample_t*)*output_channels);
    ja_check_ptr(ja_outputs);

    if (ja_register_ports() != 0)
        return 1;

    jack_set_process_thread(client, ja_thread, NULL);
    jack_on_shutdown(client, ja_shutdown, NULL);

    /* Unblock signals */
    sigemptyset(&sset);
    if (sigprocmask(SIG_SETMASK, &sset, NULL) < 0) {
        ja_error("Unblock signals error\n");
        return 1;
    }
    sigemptyset(&sig_stop_for_gc);
    sigaddset(&sig_stop_for_gc, SBCL_SIG_STOP_FOR_GC);

    ja_lisp_busy = 1;

    return 0;
}

static void ja_shutdown(void *arg)
{
    (void) arg;

    ja_status = __JA_SHUTDOWN;
    fprintf(stderr, "JACK Audio shutdown\n");
    /*
     * We are blocking SIG_STOP_FOR_GC in `ja_cycle_begin', and
     * `jack_cycle_wait' blocks if JACK has shut down. The only
     *  safe thing to do with the actual gc is to quit.
     */
    kill(getpid(), SIGQUIT);
}

static void ja_terminate(void *arg)
{
    (void) arg;

    if (ja_status != __JA_STOPPED) {
        int i;

        ja_status = __JA_STOPPED;

        if (client != NULL) {
            jack_deactivate(client);
            jack_client_close(client);
            client = NULL;
        }
        ja_free(ja_inputs);
        ja_free(ja_outputs);
        ja_free(input_ports);
        ja_free(output_ports);

        for (i=0; i<ja_in_channels; i++)
            free(input_port_names[i]);
        ja_free(input_port_names);

        for (i=0; i<ja_out_channels; i++)
            free(output_port_names[i]);
        ja_free(output_port_names);
    }
}

int ja_start()
{
    if (client == NULL) {
        ja_error("JACK client not initialized");
        return 1;
    }
    ja_status = __JA_RUNNING;
    if (jack_activate(client) != 0) {
        ja_error("error activating JACK client");
        return 1;
    }
    ja_connect_client();

    return 0;
}

int ja_stop()
{
    ja_terminate(NULL);
    return 0;
}

/* Get a frame of the input */
void ja_get_input(SAMPLE *inputs)
{
    int i;

    for (i=0; i<ja_in_channels; i++)
        inputs[i] = (SAMPLE) *ja_inputs[i]++;
}

/* Set a frame of the output */
void ja_set_output(SAMPLE *outputs)
{
    int i;

    for (i=0; i<ja_out_channels; i++) {
        *ja_outputs[i]++ = (jack_default_audio_sample_t) outputs[i];
        outputs[i] = (jack_default_audio_sample_t) 0.0;
    }
}
