/*
 * Copyright (c) 2013-2020 Tito Latini
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
#include "rtpa.h"

static void pa_set_error_msg(const char *msg)
{
        strncpy(pa_error_msg, msg, PA_ERROR_MSG_MAX_LENGTH);
}

static void pa_error(const char *msg)
{
        pa_set_error_msg(msg);
        pa_stop();
}

static int pa_alloc_input_cache(void)
{
        if (pa_in_channels > 0) {
                unsigned int size;

                size = (unsigned int) (pa_sample_rate * pa_in_channels
                                       * MAX_GC_TIME_SEC * PA_SAMPLE_SIZE);
                pa_inputs_cache = incudine_ringbuffer_create(size);
                RETURN_IF_NULLPTR(pa_inputs_cache,
                                  "incudine_ringbuffer_create() failure");
        }
        /* Cycle counter: 1 byte = 1 cycle. */
        pa_cycle_counter = incudine_ringbuffer_create(PA_MAX_RECOVERED_CYCLES);
        RETURN_IF_NULLPTR(pa_cycle_counter,
                          "incudine_ringbuffer_create() failure");
        return 0;
}

static void pa_free_input_cache(void)
{
        if (pa_inputs_cache != NULL) {
                incudine_ringbuffer_free(pa_inputs_cache);
                pa_inputs_cache = NULL;
        }
        if (pa_cycle_counter != NULL) {
                incudine_ringbuffer_free(pa_cycle_counter);
                pa_cycle_counter = NULL;
        }
}

static void pa_increment_cycle_counter(char type)
{
        if (pa_cycle_counter != NULL) {
                /*
                 * The buffer value informs if the cycle is the last.
                 * type is PA_SUSPENDED_CYCLE or PA_CONTINUE_LAST_CYCLE.
                 */
                incudine_ringbuffer_write(pa_cycle_counter, &type, 1);
        }
}

int pa_is_last_cycle(void)
{
        if (pa_has_cached_inputs()) {
                int n;
                char c;

                n = incudine_ringbuffer_peek(pa_cycle_counter, &c, 1);
                if ((n == 1) && (c == PA_CONTINUE_LAST_CYCLE))
                        return 1;
        }
        return 0;
}

int pa_cache_inputs(void)
{
        int n;

        n = 0;
        if (pa_inputs_cache != NULL) {
                n = incudine_ringbuffer_write(pa_inputs_cache,
                                              (char *) pa_inputs,
                                              pa_inbuf_bytes);
        }
        pa_increment_cycle_counter(PA_SUSPENDED_CYCLE);
        return n;
}

int pa_has_cached_inputs(void)
{
        int n = 1;

        if (pa_cycle_counter != NULL)
                n = incudine_ringbuffer_empty(pa_cycle_counter);
        return (n == 0);
}

void pa_clear_cached_inputs(void)
{
        if (pa_inputs_cache != NULL)
                incudine_ringbuffer_reset(pa_inputs_cache);
        if (pa_cycle_counter != NULL)
                incudine_ringbuffer_reset(pa_cycle_counter);
}

/*
 * Retrieve the cached inputs.
 * The cycle counter is not updated.
*/
int pa_inputs_from_cache_begin(void)
{
        int n = 0;

        if (pa_inputs_cache != NULL) {
                SAMPLE *tmp;
                float *inputs;
                int i;

                n = incudine_ringbuffer_read(pa_inputs_cache,
                                             (char *) pa_tmp_inputs,
                                             pa_inbuf_bytes);
                tmp = lisp_input;
                inputs = pa_tmp_inputs;
                for (i = 0; i < pa_inbuf_samples; i++)
                        *tmp++ = (SAMPLE) *inputs++;
        }
        return n;
}

/* Update the cycle counter after pa_inputs_from_cache_begin(). */
int pa_inputs_from_cache_end(void)
{
        if (pa_cycle_counter != NULL) {
                int n;
                char c;
                n = incudine_ringbuffer_read(pa_cycle_counter, &c, 1);
                return n;
        }
        return 0;
}

static void *pa_process_thread(void *arg)
{
        PaError err;
        (void) arg;

        err = Pa_StartStream(stream);
        if (err != paNoError) {
                fprintf(stderr, "Pa_StartStream() error: %s\n",
                        Pa_GetErrorText(err));
                return 0;
        }
        pa_status = PA_RUNNING;

        while(pa_status == PA_RUNNING) {
                int lisp_busy;

                lisp_busy = pa_lisp_busy;
                read_memory_barrier();
                if (lisp_busy) {
                        pa_cycle_begin();
                        if (pa_out_channels > 0)
                                memset(pa_outputs, 0, pa_outbuf_bytes);
                        pa_cycle_end(frames_per_buffer);
                } else {
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

static void *pa_process_thread_with_cached_inputs(void *arg)
{
        struct timespec one_sample;
        PaError err;
        int cycles;
        (void) arg;

        one_sample.tv_sec = 0;
        one_sample.tv_nsec = (long) (1e9 * pa_sample_duration);
        cycles = 0;
        err = Pa_StartStream(stream);
        if (err != paNoError) {
                fprintf(stderr, "PA_StartStream() error: %s\n",
                        Pa_GetErrorText(err));
                return 0;
        }
        pa_status = PA_RUNNING;
loop_start:
        while(pa_status == PA_RUNNING) {
                int lisp_busy;

                lisp_busy = pa_lisp_busy;
                read_memory_barrier();
                if (lisp_busy) {
                        signed long n;
                        PaError err;

                        cycles++;
                        n = (pa_in_channels > 0 ?
                             Pa_GetStreamReadAvailable(stream) :
                             Pa_GetStreamWriteAvailable(stream));
                        if (n < frames_per_buffer) {
                                /*
                                 * Ensure blocking, otherwise the recovery in
                                 * Lisp-thread is possibly slower than the
                                 * suspended cycles in C-thread.
                                 * One sample delay and try again.
                                 */
                                nanosleep(&one_sample, NULL);
                                goto loop_start;
                        }
                        if (pa_in_channels > 0) {
                                err = Pa_ReadStream(stream, pa_inputs,
                                                    frames_per_buffer);
                                if (err != paNoError && pa_out_channels == 0)
                                        pa_set_xrun();
                        }
                        /*
                         * The audio cycle could continue in Lisp after gc
                         * if there are not other cycles to recover.
                         */
                        MAYBE_CONTINUE_IN_LISP(pa_lisp_busy, resume_lisp);

                        /* Cached inputs used if it is not the last cycle. */
                        if (pa_inputs_cache != NULL) {
                                incudine_ringbuffer_write(pa_inputs_cache,
                                                          (char *) pa_inputs,
                                                          pa_inbuf_bytes);
                        }
                        MAYBE_CONTINUE_IN_LISP(pa_lisp_busy, resume_lisp);

                        /*
                         * The outputs are not zero if it is the last cycle,
                         * because it ends in Lisp.
                         */
                        if (pa_out_channels > 0)
                                memset(pa_outputs, 0, pa_outbuf_bytes);
                        MAYBE_CONTINUE_IN_LISP(pa_lisp_busy, resume_lisp);

                        /* This cycle is not the last; it is to recover. */
                        if (pa_out_channels > 0) {
                                err = Pa_WriteStream(stream, pa_outputs,
                                                     frames_per_buffer);
                                if (err != paNoError)
                                        pa_set_xrun();
                        }
                        pa_increment_cycle_counter(PA_SUSPENDED_CYCLE);
                } else {
                resume_lisp:
                        /* We may be here also after a one sample delay. */
                        if ((cycles > 0) &&
                            (!(pa_has_cached_inputs()))) {
                                pa_increment_cycle_counter(PA_CONTINUE_LAST_CYCLE);
                        }
                        /*
                         * Continue the last cycle in Lisp-thread and block
                         * the current C-thread.
                         */
                        __pa_condition_signal(&pa_lisp_cond, &pa_lisp_lock);
                        __pa_condition_wait(&pa_c_cond, &pa_c_lock);

                        pa_cycle_start_time_sec = Pa_GetStreamTime(stream);
                        pa_cycle_start_time_smp = *pa_sample_counter;
                        cycles = 0;
                }
        }
        return 0;
}

char *pa_get_error_msg(void)
{
        return pa_error_msg;
}

/* Wait on the lisp realtime thread */
void pa_condition_wait(void)
{
        __pa_condition_wait(&pa_lisp_cond, &pa_lisp_lock);
}

/* Lisp rt thread is busy ? */
void pa_set_lisp_busy_state(int status)
{
        write_memory_barrier();
        pa_lisp_busy = status;
}

/* Transfer the control of the client to C realtime thread */
void pa_transfer_to_c_thread(void)
{
        write_memory_barrier();
        pa_lisp_busy = TRUE;
        __pa_condition_signal(&pa_c_cond, &pa_c_lock);
}

int pa_set_thread_callback(int has_cached_inputs)
{
        pa_thread_callback = (has_cached_inputs ?
                              pa_process_thread_with_cached_inputs :
                              pa_process_thread);
        return has_cached_inputs;
}

int pa_get_buffer_size(void)
{
        return frames_per_buffer;
}

SAMPLE pa_get_sample_rate(void)
{
        return pa_sample_rate;
}

static int pa_set_xrun(void)
{
        pa_xruns.count++;
        pa_xruns.last_time = *pa_sample_counter;
        return 0;
}

struct pa_xrun *pa_get_xruns(void)
{
        return &pa_xruns;
}

void pa_xrun_reset(void)
{
        pa_xruns.count = 0;
        pa_xruns.last_time = (SAMPLE) 0.0;
}

int pa_initialize(unsigned int input_channels, unsigned int output_channels,
                  unsigned long nframes, const char* client_name,
                  SAMPLE *sample_counter)
{
        double srate = -1.0;
        PaStreamParameters input_param, *iparam = NULL;
        PaStreamParameters output_param, *oparam = NULL;
        PaStreamInfo *stream_info;
        PaDeviceIndex count;
        PaError err;
        sigset_t sset;

#ifdef PA_HAVE_JACK
        PaJack_SetClientName(client_name);    
#else
        (void) client_name;
#endif

        if (input_channels == 0 && output_channels == 0) {
                pa_set_error_msg(
                        "Zero input channels and zero output channels.");
                return 1;
        }
        err = Pa_Initialize();
        if (err != paNoError) {
                pa_set_error_msg(Pa_GetErrorText(err));
                fprintf(stderr, "Pa_Initialize() failed.\n");
                return 1;
        }
        pa_status = PA_INITIALIZING;
        pa_in_channels = input_channels;
        pa_out_channels = output_channels;
        count = Pa_GetDeviceCount();
        if (input_channels > 0) {
                if (pa_input_id >= 0 && pa_input_id < count) {
                        input_param.device = pa_input_id;
                } else {
                        input_param.device = Pa_GetDefaultInputDevice();
                }
                if (input_param.device != paNoDevice) {
                        const PaDeviceInfo *info;
                        info = Pa_GetDeviceInfo(input_param.device);
                        input_param.channelCount = input_channels;
                        input_param.sampleFormat = paFloat32;
                        input_param.suggestedLatency =
                                info->defaultLowInputLatency;
                        input_param.hostApiSpecificStreamInfo = NULL;
                        iparam = &input_param;
                        srate = info->defaultSampleRate;
                }
        }
        if (output_channels > 0) {
                if (pa_output_id >= 0 && pa_output_id < count) {
                        output_param.device = pa_output_id;
                } else {
                        output_param.device = Pa_GetDefaultOutputDevice();
                }
                if (output_param.device != paNoDevice) {
                        const PaDeviceInfo *info;
                        info = Pa_GetDeviceInfo(output_param.device);
                        output_param.channelCount = output_channels;
                        output_param.sampleFormat = paFloat32;
                        output_param.suggestedLatency =
                                info->defaultLowOutputLatency;
                        output_param.hostApiSpecificStreamInfo = NULL;
                        oparam = &output_param;
                        if (srate <= 0)
                                srate = info->defaultSampleRate;
                }
        }
        err = Pa_OpenStream(&stream, iparam, oparam, srate, nframes, paClipOff,
                            NULL, NULL);
        if (err != paNoError) {
                stream = NULL;
                pa_set_error_msg(Pa_GetErrorText(err));
                fprintf(stderr, "Pa_OpenStream() failed.\n");
                Pa_Terminate();
                return 1;
        }
        stream_info = (PaStreamInfo *) Pa_GetStreamInfo(stream);
        if (stream_info == NULL) {
                pa_set_error_msg("Pa_GetStreamInfo() failed.");
                Pa_Terminate();
                return 1;
        }
        pa_sample_rate = (SAMPLE) stream_info->sampleRate;
        pa_sample_duration = 1.0 / pa_sample_rate;
        frames_per_buffer = nframes;
        pa_outbuf_samples = nframes * output_channels;
        pa_outbuf_bytes = nframes * output_channels * sizeof(float);
        if (output_channels > 0) {
                pa_outputs = (float *) malloc(pa_outbuf_bytes);
                RETURN_IF_NULLPTR(pa_outputs, "malloc of output buffer failed");
        }
        pa_inbuf_samples = nframes * input_channels;
        pa_inbuf_bytes = pa_inbuf_samples * sizeof(float);
        if (input_channels > 0) {
                pa_inputs = (float *) malloc(pa_inbuf_bytes);
                RETURN_IF_NULLPTR(pa_inputs, "malloc of input buffer failed");
                pa_tmp_inputs = (float *) malloc(pa_inbuf_bytes);
                RETURN_IF_NULLPTR(pa_tmp_inputs,
                                  "malloc of temporary inputs failed");
        }
        err = pa_alloc_input_cache();
        /* RETURN_IF_NULLPTR called from pa_alloc_input_cache() */
        if (err) return err;
        /* Unblock signals */
        sigemptyset(&sset);
        if (sigprocmask(SIG_SETMASK, &sset, NULL) < 0) {
                pa_set_error_msg("Unblock signals error");
                Pa_Terminate();
                return 1;
        }
        if (pa_thread_callback == NULL)
                pa_set_thread_callback(PA_THREAD_CALLBACK_DEFAULT);
        pa_xrun_reset();
        pa_cycle_start_time_sec = (PaTime) 0.0;
        pa_cycle_start_time_smp = (SAMPLE) 0.0;
        pa_sample_counter = sample_counter;
        pa_lisp_busy = TRUE;
        return err;
}

int pa_start(void)
{
        int err;

        /*
         * Auxiliary C realtime thread. If lisp is busy, it continues the work
         * to avoid xruns.
         */
        err = pthread_create(&process_thread, NULL, pa_thread_callback, NULL);
        if (err) {
                pa_set_error_msg("Failed to create the C realtime thread");
                pa_stop();
                return err;
        }
        return 0;
}

int pa_stop(void)
{
        PaError err;
        int status;

        if (pa_status == PA_STOPPED)
                return 0;

        err = paNoError;
        status = pa_status;
        pa_status = PA_STOPPED;
        if (status == PA_RUNNING)
                pthread_join(process_thread, NULL);
        if (stream != NULL) {
                err = Pa_IsStreamStopped(stream);
                if (err == 0) {
                        err = Pa_StopStream(stream);
                        if (err != paNoError) {
                                pa_set_error_msg(Pa_GetErrorText(err));
                                fprintf(stderr, "PA_StopStream() failed.\n");
                        }
                }
                err = Pa_CloseStream(stream);
                if (err != paNoError) {
                        pa_set_error_msg(Pa_GetErrorText(err));
                        fprintf(stderr, "PA_CloseStream() failed.\n");
                }
                stream = NULL;
        }
        Pa_Terminate();
        pa_free(pa_outputs);
        pa_free(pa_inputs);
        pa_free(pa_tmp_inputs);
        pa_free_input_cache();

        *pa_sample_counter = (SAMPLE) 0.0;
        pa_cycle_start_time_smp = (SAMPLE) 0.0;
        pa_cycle_start_time_sec = (PaTime) 0.0;
        return err;
}

void pa_set_lisp_io(SAMPLE *input, SAMPLE *output)
{
        lisp_input = input;
        lisp_output = output;
}

unsigned long pa_cycle_begin(void)
{
        SAMPLE *tmp;
        int i;

        if (pa_status != PA_RUNNING)
                return 0;

        if (pa_in_channels > 0) {
                PaError err;

                err = Pa_ReadStream(stream, pa_inputs, frames_per_buffer);
                if (err != paNoError && pa_out_channels == 0)
                        pa_set_xrun();
                tmp = lisp_input;
                for (i = 0; i < pa_inbuf_samples; i++)
                        *tmp++ = (SAMPLE) pa_inputs[i];
        }
        pa_cycle_start_time_sec = Pa_GetStreamTime(stream);
        pa_cycle_start_time_smp = *pa_sample_counter;
        return frames_per_buffer;
}

/* Called from Lisp to continue the last cycle started from C-thread. */
void pa_continue_cycle_begin(signed long nframes)
{
        (void) nframes;

        if (pa_in_channels > 0) {
                SAMPLE *tmp;
                int i;

                tmp = lisp_input;
                for (i = 0; i < pa_inbuf_samples; i++)
                        *tmp++ = (SAMPLE) pa_inputs[i];
        }
        pa_cycle_start_time_sec = Pa_GetStreamTime(stream);
        pa_cycle_start_time_smp = *pa_sample_counter;
}

void pa_cycle_end(unsigned long nframes)
{
        SAMPLE *tmp;
        PaError err;
        int i;

        if (nframes == 0 || pa_out_channels == 0)
                return;
        tmp = lisp_output;
        for (i = 0; i < pa_outbuf_samples; i++) {
                pa_outputs[i] = (float) *tmp;
                *tmp++ = (SAMPLE) 0.0;
        }
        err = Pa_WriteStream(stream, pa_outputs, frames_per_buffer);
        if (err != paNoError)
                pa_set_xrun();
}

SAMPLE pa_get_cycle_start_time(void)
{
        return pa_cycle_start_time_smp;
}

double pa_get_time_offset_seconds(void)
{
        if (stream != NULL) {
                PaTime time_from_cycle_start;
                SAMPLE frames_to_seconds;

                time_from_cycle_start =
                        Pa_GetStreamTime(stream) - pa_cycle_start_time_sec;
                frames_to_seconds =
                        (*pa_sample_counter - pa_cycle_start_time_smp) * pa_sample_duration;
                if (frames_to_seconds > time_from_cycle_start)
                        return (double) (frames_to_seconds - time_from_cycle_start);
        }
        return 0.0;
}

uint32_t pa_get_time_offset_frames(void)
{
        if (stream != NULL) {
                PaTime time_from_cycle_start, frames;

                time_from_cycle_start =
                        Pa_GetStreamTime(stream) - pa_cycle_start_time_sec;
                frames = *pa_sample_counter - pa_cycle_start_time_smp;
                if (frames > time_from_cycle_start)
                        return (uint32_t) (frames - time_from_cycle_start);
        }
        return 0;
}

PaStream *pa_stream(void)
{
        return stream;
}

void pa_set_devices (PaDeviceIndex input, PaDeviceIndex output)
{
        pa_input_id = input;
        pa_output_id = output;
}
