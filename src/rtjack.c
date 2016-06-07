/*
 * Copyright (c) 2013-2016 Tito Latini
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
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "rtjack.h"

static void ja_set_error_msg(const char *msg)
{
        strncpy(ja_error_msg, msg, JA_ERROR_MSG_MAX_LENGTH);
}

static void ja_error(const char *msg) {
        ja_set_error_msg(msg);
        ja_stop();
}

static void ja_default_error_callback(const char *msg)
{
        fprintf(stderr, "%s\n", msg);
}

static void ja_silent_error_callback(const char *msg)
{
        (void) msg;
}

void ja_silent_errors(int silent)
{
        jack_set_error_function(silent ?
                                ja_silent_error_callback
                                : ja_default_error_callback);
}

static int ja_register_ports(void)
{
        int i;

        input_ports =
                (jack_port_t **) malloc(sizeof(jack_port_t *) * ja_in_channels);
        RETURN_IF_NULLPTR(input_ports, "malloc failure");

        input_port_names = (char **) malloc(sizeof(char *) * ja_in_channels);
        RETURN_IF_NULLPTR(input_port_names, "malloc failure");

        for (i = 0; i < ja_in_channels; i++) {
                input_port_names[i] =
                        (char *) malloc(sizeof(char) * JA_PORT_NAME_MAX_LENGTH);
                RETURN_IF_NULLPTR(input_port_names[i], "malloc failure");
                sprintf(input_port_names[i], "in_%d", i + 1);
                input_ports[i] = jack_port_register (client,
                                                     input_port_names[i],
                                                     JACK_DEFAULT_AUDIO_TYPE,
                                                     JackPortIsInput, 0);
        }

        output_ports =
                (jack_port_t **) malloc(sizeof(jack_port_t *) * ja_out_channels);
        RETURN_IF_NULLPTR(output_ports, "malloc failure");

        output_port_names = (char **) malloc(sizeof(char *) * ja_out_channels);
        RETURN_IF_NULLPTR(output_port_names, "malloc failure");

        for (i = 0; i < ja_out_channels; i++) {
                output_port_names[i] =
                        (char *) malloc(sizeof(char) * JA_PORT_NAME_MAX_LENGTH);
                RETURN_IF_NULLPTR(output_port_names[i], "malloc failure");
                sprintf(output_port_names[i], "out_%d", i + 1);
                output_ports[i] = jack_port_register (client,
                                                      output_port_names[i],
                                                      JACK_DEFAULT_AUDIO_TYPE,
                                                      JackPortIsOutput, 0);
        }
        return 0;
}

static int ja_connect_client(void)
{
        char **ports;
        int i;

        ports = (char **) jack_get_ports(client, NULL, NULL,
                                         JackPortIsPhysical | JackPortIsOutput);
        if (ports == NULL)
                return 1;

        for (i = 0; i < ja_in_channels && ports[i] != NULL; i++)
                jack_connect(client, ports[i], jack_port_name(input_ports[i]));
        jack_free(ports);

        ports = (char **) jack_get_ports(client, NULL, NULL,
                                         JackPortIsPhysical | JackPortIsInput);
        if (ports == NULL)
                return 1;

        for (i = 0; i < ja_out_channels && ports[i] != NULL; i++)
                jack_connect(client, jack_port_name(output_ports[i]), ports[i]);
        jack_free(ports);

        return 0;
}

static void* ja_process_thread(void *arg)
{
        (void) arg;

        while (ja_status == JA_RUNNING) {
                if (ja_lisp_busy) {
                        int i;
                        jack_nframes_t frames = jack_cycle_wait(client);

                        if (ja_frames != frames) {
                                /* Buffer size is changed */
                                ja_frames = frames;
                                ja_buffer_bytes = frames * JA_SAMPLE_SIZE;
                        }

                        for (i = 0; i < ja_out_channels; i++) {
                                ja_outputs[i] =
                                        jack_port_get_buffer(output_ports[i],
                                                             ja_frames);
                                /* Silence while lisp is busy */
                                memset(ja_outputs[i], 0, ja_buffer_bytes);
                        }
                        jack_cycle_signal(client, 0);
                } else {
                        /*
                         * Transfer the control of the client to lisp realtime
                         * thread and block the current thread.
                         *
                         * Notice it is called ONLY ONE TIME after the first
                         * cycle and ONLY ONE TIME after the gc in SBCL. The rt
                         * lisp thread uses `jack_cycle_wait' and `jack_cycle_signal'
                         * with the actual jack client. Practically, this thread
                         * is an emergency exit when we use an implementation of
                         * Common Lisp with a gc which stops the rt lisp thread.
                         * If the implementation of CL has a realtime gc, there
                         * aren't other transfers of the control from C to Lisp
                         * and vice versa.
                         */
                        __ja_condition_signal(&ja_lisp_cond, &ja_lisp_lock);
                        __ja_condition_wait(&ja_c_cond, &ja_c_lock);
                }
        }
        return 0;
}

static void ja_shutdown(void *arg)
{
        (void) arg;

        ja_status = JA_SHUTDOWN;
        fprintf(stderr, "JACK Audio shutdown\n");
        kill(getpid(), SIGQUIT);
}

static void ja_terminate(void *arg)
{
        (void) arg;

        if (ja_status != JA_STOPPED) {
                int i;

                ja_status = JA_STOPPED;

                if (client != NULL) {
                        jack_deactivate(client);
                        jack_client_close(client);
                        client = NULL;
                }
                ja_free(ja_inputs);
                ja_free(ja_outputs);
                ja_free(input_ports);
                ja_free(output_ports);
                ja_free(jm_pad_buffer);
                ja_free(jm_inputs);
                ja_free(jm_outputs);
                ja_free(jm_invec_tmp.data);
                ja_free(jm_outvec_tmp.data);

                for (i = 0; i < ja_in_channels; i++)
                        free(input_port_names[i]);
                ja_free(input_port_names);

                for (i = 0; i < ja_out_channels; i++)
                        free(output_port_names[i]);
                ja_free(output_port_names);
                ja_cycle_start_time = (SAMPLE) 0.0;
        }
}

char *ja_get_error_msg(void)
{
        return ja_error_msg;
}

/* Wait on the lisp realtime thread */
void ja_condition_wait(void)
{
        __ja_condition_wait(&ja_lisp_cond, &ja_lisp_lock);
}

/* Lisp rt thread is busy ? */
void ja_set_lisp_busy_state(int status)
{
        ja_lisp_busy = status;
}

/* Transfer the control of the client to C realtime thread */
void ja_transfer_to_c_thread(void)
{
        ja_lisp_busy = 1;
        __ja_condition_signal(&ja_c_cond, &ja_c_lock);
}

int ja_get_buffer_size(void)
{
        return ja_frames;
}

SAMPLE ja_get_sample_rate(void)
{
        return ja_sample_rate;
}

int ja_initialize(SAMPLE srate, unsigned int input_channels,
                  unsigned int output_channels, unsigned int nframes,
                  const char* client_name, SAMPLE *sample_counter)
{
        sigset_t sset;
        (void) srate;
        (void) nframes;

        ja_error_msg[0] = '\0';
        client = jack_client_open(client_name, JackNoStartServer, NULL);
        if (client == NULL) {
                ja_set_error_msg("jack_client_open failure");
                return 1;
        }
        ja_frames = jack_get_buffer_size(client);
        ja_sample_rate = (SAMPLE) jack_get_sample_rate(client);
        ja_in_channels = input_channels;
        ja_out_channels = output_channels;
        ja_buffer_bytes = ja_frames * JA_SAMPLE_SIZE;
        ja_cycle_start_time = (SAMPLE) 0.0;
        ja_sample_counter = sample_counter;

        ja_inputs = (jack_default_audio_sample_t **)
                malloc(sizeof(jack_default_audio_sample_t *) * input_channels);
        RETURN_IF_NULLPTR(ja_inputs, "malloc failure");

        ja_outputs = (jack_default_audio_sample_t **)
                malloc(sizeof(jack_default_audio_sample_t *) * output_channels);
        RETURN_IF_NULLPTR(ja_outputs, "malloc failure");

        jm_pad_buffer = (char *) malloc(JM_RINGBUFFER_SIZE);
        RETURN_IF_NULLPTR(jm_pad_buffer, "malloc failure");

        jm_inputs = (struct jm_data **)
                malloc(JM_INITIAL_MAX_PORTS * sizeof(struct jm_data *));
        RETURN_IF_NULLPTR(jm_inputs, "malloc failure");
        memset(jm_inputs, 0, JM_INITIAL_MAX_PORTS * sizeof(struct jm_data *));

        jm_outputs = (struct jm_data **)
                malloc(JM_INITIAL_MAX_PORTS * sizeof(struct jm_data *));
        RETURN_IF_NULLPTR(jm_outputs, "malloc failure");
        memset(jm_outputs, 0, JM_INITIAL_MAX_PORTS * sizeof(struct jm_data *));

        jm_invec_tmp.data = (struct jm_data **)
                malloc(JM_INITIAL_MAX_PORTS * sizeof(struct jm_data *));
        RETURN_IF_NULLPTR(jm_invec_tmp.data, "malloc failure");
        memset(jm_invec_tmp.data, 0, JM_INITIAL_MAX_PORTS * sizeof(struct jm_data *));
        jm_invec_tmp.count = 0;
        jm_invec_tmp.size = JM_INITIAL_MAX_PORTS;

        jm_outvec_tmp.data = (struct jm_data **)
                malloc(JM_INITIAL_MAX_PORTS * sizeof(struct jm_data *));
        RETURN_IF_NULLPTR(jm_outvec_tmp.data, "malloc failure");
        memset(jm_outvec_tmp.data, 0, JM_INITIAL_MAX_PORTS * sizeof(struct jm_data *));
        jm_outvec_tmp.count = 0;
        jm_outvec_tmp.size = JM_INITIAL_MAX_PORTS;

        if (ja_register_ports() != 0)
                return 1;

        jack_set_process_thread(client, ja_process_thread, NULL);
        jack_on_shutdown(client, ja_shutdown, NULL);

        /* Unblock signals */
        sigemptyset(&sset);
        if (sigprocmask(SIG_SETMASK, &sset, NULL) < 0) {
                ja_error("Unblock signals error\n");
                return 1;
        }
        ja_lisp_busy = 1;

        return 0;
}

int ja_start(void)
{
        if (client == NULL) {
                ja_error("JACK client not initialized");
                return 1;
        }
        ja_status = JA_RUNNING;
        if (jack_activate(client) != 0) {
                ja_error("error activating JACK client");
                return 1;
        }
        ja_connect_client();

        return 0;
}

int ja_stop(void)
{
        ja_terminate(NULL);
        return 0;
}

void ja_set_lisp_io(SAMPLE *input, SAMPLE *output)
{
        lisp_input = input;
        lisp_output = output;
}

jack_nframes_t ja_cycle_begin(void)
{
        int i;
        jack_nframes_t frames;

        if (ja_status != JA_RUNNING)
                return 0;
        /*
         * In JACK2, `sem_timedwait' is interrupted by SIGUSR2 during the gc.
         * We are within SB-SYS:WITHOUT-GCING but the inhibition of the gc is
         * not guaranteed, therefore it is not a good idea to block this signal
         * around `jack_cycle_wait', because it could cause the arrest of SBCL.
         */
        frames = jack_cycle_wait(client);

        if (ja_status != JA_RUNNING)
                return 0;

        for (i = 0; i < ja_in_channels; i++) {
                int j;
                SAMPLE *tmp;

                ja_inputs[i] = jack_port_get_buffer(input_ports[i], frames);
                tmp = lisp_input + i;
                for (j = 0; j < frames; j++) {
                        *tmp = (SAMPLE) ja_inputs[i][j];
                        tmp += ja_in_channels;
                }
        }
        for (i = 0; i < ja_out_channels; i++)
                ja_outputs[i] = jack_port_get_buffer(output_ports[i], frames);

        ja_cycle_start_time = *ja_sample_counter;

        return frames;
}

void ja_cycle_end(jack_nframes_t frames)
{
        int i, j;
        SAMPLE *output = lisp_output;

        for (i = 0; i < frames; i++) {
                for (j = 0; j < ja_out_channels; j++) {
                        ja_outputs[j][i] = (jack_default_audio_sample_t) *output;
                        *output++ = (SAMPLE) 0.0;
                }
        }
        jack_cycle_signal(client, 0);
}

SAMPLE ja_get_cycle_start_time(void)
{
        return ja_cycle_start_time;
}

jack_client_t *ja_client(void)
{
        return client;
}


/* Jack MIDI */

jack_port_t *jm_port_register(const char *port_name, int is_input)
{
        return jack_port_register(client, port_name, JACK_DEFAULT_MIDI_TYPE,
                                  is_input ? JackPortIsInput : JackPortIsOutput,
                                  0);
}

struct jm_data *jm_alloc_data(int is_input)
{
        struct jm_data *p;
        size_t size;

        size = (is_input == 0 ? sizeof(struct jm_data)
                              : sizeof(struct jm_input_data));
        p = (struct jm_data *) malloc(size);
        if (p != NULL) {
                memset(p, 0, size);
                p->rb = jack_ringbuffer_create(JM_RINGBUFFER_SIZE);
                if (is_input == 0) {
                        if (p->rb == NULL) {
                                free(p);
                                return NULL;
                        }
                } else {
                        struct jm_input_data *q;
                        q = (struct jm_input_data *) p;
                        if (p->rb == NULL) {
                                free(q);
                                return NULL;
                        }
                        if ((pthread_mutex_init(&q->lock, NULL) != 0) ||
                            (pthread_cond_init(&q->cond, NULL) != 0)) {
                                jack_ringbuffer_free(q->rb);
                                free(q);
                                return NULL;
                        }
                }
        }
        return p;
}

void jm_free_data(struct jm_data *p, int is_input)
{
        if (p != NULL) {
                if (p->rb != NULL) {
                        jack_ringbuffer_free(p->rb);
                        p->rb = NULL;
                }
                if (is_input == 0) {
                        free(p);
                } else {
                        struct jm_input_data *q = (struct jm_input_data *) p;
                        pthread_mutex_destroy(&q->lock);
                        pthread_cond_destroy(&q->cond);
                        q->to_signal = 0;
                        free(q);
                }
        }
}

struct jm_data_vec *jm_copy_data_vec(int is_input)
{
        struct jm_data_vec *vec, *tmp;

        vec = (struct jm_data_vec *) malloc(sizeof(struct jm_data_vec));
        if (vec == NULL)
                return NULL;

        tmp = (is_input == 0 ? &jm_outvec_tmp : &jm_invec_tmp);

        vec->data = (struct jm_data **)
                malloc(tmp->size * sizeof(struct jm_data *));
        if (vec->data == NULL) {
                free(vec);
                return NULL;
        }
        memcpy(vec->data, tmp->data, tmp->size * sizeof(struct jm_data *));
        vec->count = tmp->count;
        vec->size = tmp->size;
        return vec;
}

void jm_free_data_vec(struct jm_data_vec *p)
{
        if (p != NULL) {
                if (p->data != NULL)
                        free(p->data);
                free(p);
        }
}

int jm_append_pending_data(struct jm_data *p, int is_input)
{
        struct jm_data **q;
        struct jm_data_vec *vec;

        vec = (is_input == 0 ? &jm_outvec_tmp : &jm_invec_tmp);

        if (vec->count >= (vec->size - 1)) {
                vec->size += JM_NUMBER_OF_PORTS_INCREMENT;
                vec->data = (struct jm_data **)
                        realloc(vec->data,
                                vec->size * sizeof(struct jm_data *));
                if (vec->data == NULL)
                        return JM_MEMORY_ERROR;
        }
        for (q = vec->data; *q != NULL; q++)
                ;
        q[0] = p;
        q[1] = NULL;
        vec->count++;
        return 0;
}

void jm_delete_from_pending_data(struct jm_data *p, int is_input)
{
        struct jm_data **q;
        struct jm_data_vec *vec;
        int found = 0;

        vec = (is_input == 0 ? &jm_outvec_tmp : &jm_invec_tmp);

        for (q = vec->data; *q != NULL; q++) {
                if (found) {
                        *q = *(q + 1);
                        if (*q == NULL)
                                return;
                } else if (*q == p) {
                        (*q)->port_buffer = NULL;
                        *q = *(q + 1);
                        found = 1;
                }
        }
}

/* Swap new data with old data in realtime thread. */
void jm_update_data(struct jm_data_vec *vec, int is_input)
{
        struct jm_data **p;

        if (is_input == 0) {
                p = jm_outputs;
                jm_outputs = vec->data;
        } else {
                p = jm_inputs;
                jm_inputs = vec->data;
        }
        vec->data = p;
}

void jm_process(jack_nframes_t frames)
{
        struct jm_data **in, **out;

        for (out = jm_outputs; *out != NULL; out++) {
                (*out)->port_buffer = jack_port_get_buffer((*out)->port, frames);
                jack_midi_clear_buffer((*out)->port_buffer);
        }
        for (in = jm_inputs; *in != NULL; in++) {
                struct jm_input_data *p;
                int i, n;
                p = (struct jm_input_data *) *in;
                p->port_buffer = jack_port_get_buffer(p->port, frames);
                if (p->port_buffer == NULL)
                        continue;

                n = jack_midi_get_event_count(p->port_buffer);
                for (i = 0; i < n; i++) {
                        jack_midi_event_t ev;
                        size_t size;
                        int ret;
                        ret = jack_midi_event_get(&ev, p->port_buffer, i);
                        if (ret != 0)
                                continue;
                        size = ev.size + JM_HEADER_SIZE;
                        if (jack_ringbuffer_write_space(p->rb) >= size) {
                                double *time;
                                uint32_t *len;
                                time = (double *) jm_pad_buffer;
                                *time = (double) (ja_cycle_start_time + ev.time);
                                len = (uint32_t *) (time + 1);
                                *len = ev.size;
                                memcpy(len + 1, ev.buffer, ev.size);
                                jack_ringbuffer_write(p->rb, jm_pad_buffer, size);
                        }
                }
                if (n > 0 || p->to_signal) {
                        if (pthread_mutex_trylock(&p->lock) == 0) {
                                pthread_cond_signal(&p->cond);
                                pthread_mutex_unlock(&p->lock);
                                p->to_signal = 0;
                        } else {
                                p->to_signal = 1;
                        }
                }
        }
}

/* Write data_size bytes of a MIDI message encoded into four bytes. */
int jm_write_short(struct jm_data *p, uint32_t msg, unsigned int data_size)
{
        jack_midi_data_t *buf;
        jack_nframes_t time;

        if (p == NULL)
                return JM_WRITE_ERROR;
        if (data_size > 4)
                return JM_DATASIZE_ERROR;

        time = (jack_nframes_t) (*ja_sample_counter - ja_cycle_start_time);
        buf = jack_midi_event_reserve(p->port_buffer, time, data_size);
        if (buf != NULL) {
                memcpy(buf, &msg, data_size);
                return 0;
        }
        return JM_WRITE_ERROR;
}

/* Write data_size bytes of a MIDI message stored into a buffer. */
int jm_write(struct jm_data *p, char *buffer, unsigned int data_size)
{
        jack_midi_data_t *jbuf;
        jack_nframes_t time;

        if (p == NULL)
                return JM_WRITE_ERROR;

        time = (jack_nframes_t) (*ja_sample_counter - ja_cycle_start_time);
        jbuf = jack_midi_event_reserve(p->port_buffer, time, data_size);
        if (jbuf != NULL) {
                memcpy(jbuf, buffer, data_size);
                return 0;
        }
        return JM_WRITE_ERROR;
}

/* Fill a buffer with the events received from a MIDI input port. */
int jm_read(struct jm_input_data *p, char *buffer, unsigned int bufsize)
{
        char *b;
        size_t n;
        int count, remain, ret;
        uint32_t ev_size, len, pad;

        if (p == NULL)
                return 0;

        b = buffer;
        count = 0;
        remain = bufsize;
        n = jack_ringbuffer_read_space(p->rb);
        if (n > 0)
                memset(b, 0, bufsize);
        while (n > 0 && remain > JM_HEADER_SIZE) {
                ret = jack_ringbuffer_peek(p->rb, (char *) b, JM_HEADER_SIZE);
                /* Message length. */
                len = ((uint32_t *) b)[2];
                if (ret != JM_HEADER_SIZE || len == 0) {
                        /* Corrupted data. */
                        jm_flush_pending((struct jm_data *) p);
                        break;
                }
                /* Alignment to four bytes. */
                pad = ((int) (-len) & 3);
                ev_size = len + JM_HEADER_SIZE;
                if (ev_size < remain) {
                        ret = jack_ringbuffer_read(p->rb, b, ev_size);
                        ev_size += pad;
                        b += ev_size;
                        remain -= ev_size;
                        count++;
                } else if (ev_size > bufsize) {
                        /* Ignore the event. */
                        jack_ringbuffer_read_advance(p->rb, ev_size);
                } else {
                        /* The buffer is full. */
                        break;
                }
                n = jack_ringbuffer_read_space(p->rb);
        }
        return count;
}

void jm_flush_pending(struct jm_data *p)
{
        if (p != NULL) {
                size_t n;
                n = jack_ringbuffer_read_space(p->rb);
                if (n > 0)
                        jack_ringbuffer_read_advance(p->rb, n);
        }
}

void jm_waiting_for(struct jm_input_data *p)
{
        if (p != NULL) {
                pthread_mutex_lock(&p->lock);
                pthread_cond_wait(&p->cond, &p->lock);
                pthread_mutex_unlock(&p->lock);
        }
}

/* Used in lisp during RECV-STOP. */
void jm_force_cond_signal(struct jm_input_data *p)
{
        if (p != NULL) {
                pthread_mutex_lock(&p->lock);
                pthread_cond_signal(&p->cond);
                pthread_mutex_unlock(&p->lock);
                p->to_signal = 0;
        }
}
