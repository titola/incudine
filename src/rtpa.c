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
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <pthread.h>
#include <portaudio.h>
#include "common.h"

#ifdef PA_HAVE_JACK
#include <pa_jack.h>
#endif

enum {
        PA_RUNNING,
        PA_STOPPED
};

#define PA_ERROR_MSG_MAX_LENGTH  (256)

static unsigned int pa_in_channels, pa_out_channels, frames_per_buffer;
static float *pa_inputs, *pa_outputs;
static float *pa_inputs_anchor, *pa_outputs_anchor;
static PaStream *stream;
static size_t pa_outbuf_bytes;
static int pa_status = PA_STOPPED;
static int pa_lisp_busy;
static char pa_error_msg[PA_ERROR_MSG_MAX_LENGTH];
static pthread_t process_thread;
static pthread_mutex_t pa_lisp_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  pa_lisp_cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t pa_c_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  pa_c_cond = PTHREAD_COND_INITIALIZER;

static void pa_set_error_msg(const char *msg)
{
        strncpy(pa_error_msg, msg, PA_ERROR_MSG_MAX_LENGTH);
}

char *pa_get_error_msg(void)
{
        return pa_error_msg;
}

int pa_get_buffer_size(void)
{
        return frames_per_buffer;
}

unsigned int pa_cycle_begin(void)
{
        if (pa_status != PA_RUNNING)
                return 0;

        pa_inputs = pa_inputs_anchor;
        Pa_ReadStream(stream, pa_inputs, frames_per_buffer);
        return frames_per_buffer;
}

void pa_cycle_end(unsigned int nframes)
{
        pa_outputs = pa_outputs_anchor;
        Pa_WriteStream(stream, pa_outputs, nframes);
}

/* Lisp rt thread is busy ? */
void pa_set_lisp_busy_state(int status)
{
        pa_lisp_busy = status;
}

#define __pa_condition_wait(c, l)                       \
        do {                                            \
                pthread_mutex_lock(l);                  \
                pthread_cond_wait(c, l);                \
                pthread_mutex_unlock(l);                \
        } while (0)

#define __pa_condition_signal(c, l)                     \
        do {                                            \
                pthread_mutex_lock(l);                  \
                pthread_cond_signal(c);                 \
                pthread_mutex_unlock(l);                \
        } while (0)

/* Wait on the lisp realtime thread */
void pa_condition_wait(void)
{
        __pa_condition_wait(&pa_lisp_cond, &pa_lisp_lock);
}

/* Transfer the control of the client to C realtime thread */
void pa_transfer_to_c_thread(void)
{
        pa_lisp_busy = 1;
        __pa_condition_signal(&pa_c_cond, &pa_c_lock);
}

void *pa_process_thread(void *arg)
{
        (void) arg;

        if (Pa_StartStream(stream) != paNoError) {
                fprintf(stderr, "PA_StartStream failed\n");
                return 0;
        }
        pa_status = PA_RUNNING;

        while(1) {
                if (pa_status != PA_RUNNING)
                        return 0;

                if (pa_lisp_busy != 0) {
                        memset(pa_outputs_anchor, 0, pa_outbuf_bytes);
                        pa_cycle_end(frames_per_buffer);
                        pa_cycle_begin();
                } else {
                        memset(pa_outputs_anchor, 0, pa_outbuf_bytes);
                        pa_cycle_end(frames_per_buffer);
                        /*
                         * Transfer the control of the client to lisp realtime
                         * thread and block the current thread.
                         * Notice it is called ONLY ONE TIME after the first
                         * cycle and ONLY ONE TIME after the gc in SBCL.
                         * Practically, this thread is an emergency exit when we
                         * use an implementation of Common Lisp with a gc which
                         * stops the rt lisp thread. If the implementation of CL
                         * has a realtime gc, there aren't other transfers of
                         * the control from C to Lisp and vice versa.
                         */
                        __pa_condition_signal(&pa_lisp_cond, &pa_lisp_lock);
                        __pa_condition_wait(&pa_c_cond, &pa_c_lock);
                }
        }
        return 0;
}

int pa_initialize(SAMPLE srate, unsigned int input_channels,
                  unsigned int output_channels, unsigned int nframes,
                  const char* client_name)
{
        PaStreamParameters input_param, *iparam = NULL;
        PaStreamParameters output_param, *oparam = NULL;
        PaError err;
        sigset_t sset;

#ifdef PA_HAVE_JACK
        PaJack_SetClientName(client_name);    
#else
        (void) client_name;
#endif

        if (Pa_Initialize() != paNoError) {
                pa_set_error_msg("Pa_Initialize failed");
                return 1;
        }
        pa_in_channels = input_channels;
        pa_out_channels = output_channels;

        input_param.device = Pa_GetDefaultInputDevice();
        if (input_param.device != paNoDevice) {
                input_param.channelCount = input_channels;
                input_param.sampleFormat = paFloat32;
                input_param.suggestedLatency =
                        Pa_GetDeviceInfo(input_param.device)->defaultLowInputLatency;
                input_param.hostApiSpecificStreamInfo = NULL;
                iparam = &input_param;
        }
        output_param.device = Pa_GetDefaultOutputDevice();
        if (output_param.device != paNoDevice) {
                output_param.channelCount = output_channels;
                output_param.sampleFormat = paFloat32;
                output_param.suggestedLatency =
                        Pa_GetDeviceInfo(output_param.device)->defaultLowOutputLatency;
                output_param.hostApiSpecificStreamInfo = NULL;
                oparam = &output_param;
        }
        err = Pa_OpenStream(&stream, iparam, oparam, (double)srate, nframes,
                            paClipOff, NULL, NULL);
        if (err != paNoError) {
                pa_set_error_msg("PA_OpenStream failed");
                Pa_Terminate();
                return 1;
        }
        frames_per_buffer = nframes;
        /* Unblock signals */
        sigemptyset(&sset);
        if (sigprocmask(SIG_SETMASK, &sset, NULL) < 0) {
                pa_set_error_msg("Unblock signals error");
                Pa_Terminate();
                return 1;
        }
        pa_inputs = (float *) malloc(nframes * input_channels * sizeof(float));
        if (pa_inputs == NULL) {
                pa_set_error_msg("malloc of input buffer failed");
                Pa_Terminate();
                return 1;
        }
        pa_outbuf_bytes = nframes * output_channels * sizeof(float);
        pa_outputs = (float *) malloc(pa_outbuf_bytes);
        if (pa_outputs == NULL) {
                pa_set_error_msg("malloc of output buffer failed");
                Pa_Terminate();
                free(pa_inputs);
                return 1;
        }
        pa_inputs_anchor = pa_inputs;
        pa_outputs_anchor = pa_outputs;
        pa_lisp_busy = 1;
        return err;
}

int pa_stop(void *arg)
{
        PaError err;
        (void) arg;

        if (pa_status != PA_RUNNING)
                return 0;

        pa_status = PA_STOPPED;
        pthread_join(process_thread, NULL);

        if ((err = Pa_StopStream(stream)) != paNoError)
                pa_set_error_msg("PA_StopStream failed");
        else if ((err = Pa_CloseStream(stream)) != paNoError)
                pa_set_error_msg("PA_CloseStream failed");

        Pa_Terminate();

        if (pa_inputs_anchor != NULL) {
                free(pa_inputs_anchor);
                pa_inputs_anchor = NULL;
        }
        if (pa_outputs_anchor != NULL) {
                free(pa_outputs_anchor);
                pa_inputs_anchor = NULL;
        }
        return err;
}

int pa_start(void)
{
        int err;

        /*
         * Auxiliary C realtime thread. If lisp is busy, it continues the work
         * to avoid xruns.
         */
        if ((err = pthread_create(&process_thread, NULL, pa_process_thread,
                                  NULL))) {
                pa_set_error_msg("Failed to create the C realtime thread");
                pa_stop(NULL);
                return err;
        }
        return 0;
}

void pa_get_input(SAMPLE *inputs)
{
        int i;

        for (i = 0; i < pa_in_channels; i++)
                inputs[i] = (SAMPLE) *pa_inputs++;
}

void pa_set_output(SAMPLE *outputs)
{
        int i;

        for (i = 0; i < pa_out_channels; i++) {
                *pa_outputs++ = (float) outputs[i];
                outputs[i] = 0.0f;
        }
}
