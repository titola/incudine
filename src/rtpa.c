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
#include <signal.h>
#include <string.h>
#include <pthread.h>
#include <portaudio.h>
#include "common.h"

#ifdef PA_HAVE_JACK
#include <pa_jack.h>
#endif

static SAMPLE pa_sample_rate;
static unsigned int pa_in_channels, pa_out_channels, frames_per_buffer;
static float *pa_inputs, *pa_outputs;
static float *pa_inputs_anchor, *pa_outputs_anchor;
static PaStream *stream;
static char pa_error_msg[256];

static void pa_set_error_msg(const char *msg)
{
    strncpy(pa_error_msg, msg, 256);
}

char *pa_get_error_msg()
{
    return pa_error_msg;
}

int pa_get_buffer_size()
{
    return frames_per_buffer;
}

void pa_cycle_begin()
{
    pa_inputs = pa_inputs_anchor;
    Pa_ReadStream(stream, pa_inputs, frames_per_buffer);
}

void pa_cycle_end()
{
    pa_outputs = pa_outputs_anchor;
    Pa_WriteStream(stream, pa_outputs, frames_per_buffer);
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
                        nframes, paClipOff, NULL, NULL);
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
    pa_inputs = (float*) malloc(nframes*input_channels*sizeof(float));
    if (pa_inputs == NULL) {
        pa_set_error_msg("malloc of input buffer failed");
        Pa_Terminate();
        return 1;
    }
    pa_outputs = (float*) malloc(nframes*output_channels*sizeof(float));
    if (pa_outputs == NULL) {
        pa_set_error_msg("malloc of output buffer failed");
        Pa_Terminate();
        free(pa_inputs);
        return 1;
    }
    pa_inputs_anchor = pa_inputs;
    pa_outputs_anchor = pa_outputs;

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
    free(pa_inputs_anchor);
    free(pa_outputs_anchor);

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
        *pa_outputs++ = (float) outputs[i];
        outputs[i] = 0.0f;
    }
}
