/*
 * Copyright (c) 2013-2018 Tito Latini
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

#ifndef __RTPA_H
#define __RTPA_H

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

static unsigned int pa_in_channels, pa_out_channels;
static SAMPLE pa_sample_rate, pa_sample_duration;
static unsigned long frames_per_buffer;
static float *pa_inputs, *pa_outputs;
static float *pa_inputs_anchor, *pa_outputs_anchor;
static SAMPLE *lisp_input, *lisp_output;
static PaStream *stream = NULL;
static PaDeviceIndex pa_input_id = -1;
static PaDeviceIndex pa_output_id = -1;
static size_t pa_outbuf_bytes, pa_frame_bytes;
static SAMPLE *pa_sample_counter;
static SAMPLE pa_cycle_start_time_smp;
static PaTime pa_cycle_start_time_sec;
static int pa_status = PA_STOPPED;
static int pa_lisp_busy;
static char pa_error_msg[PA_ERROR_MSG_MAX_LENGTH];
static pthread_t process_thread;
static pthread_mutex_t pa_lisp_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  pa_lisp_cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t pa_c_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  pa_c_cond = PTHREAD_COND_INITIALIZER;

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

static void pa_set_error_msg(const char *msg);
static void *pa_process_thread(void *arg);

char *pa_get_error_msg(void);
void pa_condition_wait(void);
void pa_set_lisp_busy_state(int status);
void pa_transfer_to_c_thread(void);
int pa_get_buffer_size(void);
SAMPLE pa_get_sample_rate(void);
int pa_initialize(unsigned int input_channels, unsigned int output_channels,
                  unsigned long nframes, const char* client_name,
                  SAMPLE *sample_counter);
int pa_start(void);
int pa_stop(void *arg);
void pa_set_lisp_io(SAMPLE *input, SAMPLE *output);
unsigned long pa_cycle_begin(void);
void pa_cycle_end(unsigned long nframes);
SAMPLE pa_get_cycle_start_time(void);
double pa_get_time_offset(void);
PaStream *pa_stream(void);
void pa_set_devices(PaDeviceIndex input, PaDeviceIndex output);

#endif  /* __RTPA_H */
