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
#include <pthread.h>
#include <portaudio.h>
#include "common.h"

#ifdef PA_HAVE_JACK
#include <pa_jack.h>
#endif

SAMPLE pa_sample_rate;
unsigned int pa_in_channels, pa_out_channels;
float *pa_inputs, *pa_outputs;
PaStream *stream;
pthread_mutex_t pa_lisp_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  pa_lisp_cond = PTHREAD_COND_INITIALIZER;
char pa_error_msg[256];

void pa_set_error_msg(const char *msg)
{
    strncpy(pa_error_msg, msg, 256);
}

char *pa_get_error_msg()
{
    return pa_error_msg;
}

int pa_cb(const void *input_buffer, void *output_buffer,
          unsigned long frames_per_buffer,
          const PaStreamCallbackTimeInfo* time_info,
          PaStreamCallbackFlags status_flags, void *userdata)
{
    (void) frames_per_buffer;
    (void) time_info;
    (void) status_flags;
    (void) userdata;

    pa_inputs = (float*) input_buffer;
    pa_outputs = (float*) output_buffer;
    /*
     * Sync with the lisp thread.
     * glibc uses futex on Linux; is it safe on the other OS ?
     */
    pthread_mutex_lock(&pa_lisp_lock);
    pthread_cond_signal(&pa_lisp_cond);
    pthread_mutex_unlock(&pa_lisp_lock);
    return paContinue;
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
    pa_sample_rate = srate;
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
    err = Pa_OpenStream(&stream, iparam, oparam, (unsigned int) srate,
                        nframes, paClipOff, pa_cb, NULL);
    if (err != paNoError) {
        pa_set_error_msg("PA_OpenStream failed");
        Pa_Terminate();
        return 1;
    }
    /* Unblock signals */
    sigemptyset(&sset);
    if (sigprocmask(SIG_SETMASK, &sset, NULL) < 0) {
        pa_set_error_msg("Unblock signals error");
        Pa_Terminate();
        return 1;
    }
    return err;
}

int pa_stop(void *arg)
{
    PaError err;
    (void) arg;

    if ((err = Pa_StopStream(stream)) != paNoError)
        pa_set_error_msg("PA_StopStream failed");
    else if ((err = Pa_CloseStream(stream)) != paNoError)
        pa_set_error_msg("PA_CloseStream failed");

    Pa_Terminate();
    return err;
}

int pa_start()
{
    int err = 0;

    if (Pa_StartStream(stream) != paNoError) {
        pa_set_error_msg("PA_StartStream failed");
        err = pa_stop(NULL);
    }
    return err;
}

void pa_get_input(SAMPLE *inputs)
{
    int i;

    for (i=0; i<pa_in_channels; i++)
        inputs[i] = (SAMPLE) *pa_inputs++;
}

void pa_set_output(SAMPLE *outputs)
{
    int i;

    for (i=0; i<pa_out_channels; i++) {
        *pa_outputs++ = (float)outputs[i];
        outputs[i] = 0.0f;
    }
}

SAMPLE pa_get_realtime()
{
    return (SAMPLE)(Pa_GetStreamTime(stream) * pa_sample_rate);
}

int pa_condition_wait()
{
    pthread_mutex_lock(&pa_lisp_lock);
    pthread_cond_wait(&pa_lisp_cond, &pa_lisp_lock);
    pthread_mutex_unlock(&pa_lisp_lock);
    return 0;
}
