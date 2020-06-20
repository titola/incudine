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

#ifndef __RTPA_H
#define __RTPA_H

#include <stdint.h>
#include <pthread.h>
#include <portaudio.h>
#include <time.h>
#include "barrier.h"
#include "common.h"
#include "ringbuffer.h"

#ifdef PA_HAVE_JACK
#include <pa_jack.h>
#endif

enum {
        PA_RUNNING,
        PA_STOPPED,
        PA_INITIALIZING,
        PA_STOPPING
};

#define MAX_GC_TIME_SEC  0.33

#define PA_ERROR_MSG_MAX_LENGTH  (256)
#define PA_SAMPLE_SIZE  (sizeof(float))
#define PA_MAX_RECOVERED_CYCLES  32

#define PA_SUSPENDED_CYCLE      0
#define PA_CONTINUE_LAST_CYCLE  1

#define PA_THREAD_CALLBACK_DEFAULT  0

struct pa_xrun {
        unsigned int count;
        SAMPLE last_time;
};

struct pa_thread {
        pthread_t id;
        int status;
};

static unsigned int pa_in_channels, pa_out_channels;
static SAMPLE pa_sample_rate, pa_sample_duration;
static unsigned long frames_per_buffer;
static float *pa_inputs = NULL;
static float *pa_outputs = NULL;
static float *pa_tmp_inputs = NULL;
static struct incudine_ringbuffer *pa_inputs_cache = NULL;
static struct incudine_ringbuffer *pa_cycle_counter = NULL;
static SAMPLE *lisp_input, *lisp_output;
static PaStream *stream = NULL;
static PaDeviceIndex pa_input_id = -1;
static PaDeviceIndex pa_output_id = -1;
static size_t pa_inbuf_bytes, pa_inbuf_samples;
static size_t pa_outbuf_bytes, pa_outbuf_samples;
static SAMPLE *pa_sample_counter;
static SAMPLE pa_cycle_start_time_smp;
static PaTime pa_cycle_start_time_sec;
static PaTime pa_input_latency = 0.0;
static PaTime pa_output_latency = 0.0;
static int pa_status = PA_STOPPED;
static int pa_lisp_busy;
static struct pa_xrun pa_xruns;
static char pa_error_msg[PA_ERROR_MSG_MAX_LENGTH];
static void *(*pa_thread_callback)(void *) = NULL;
static struct pa_thread pa_proc_thread;
static pthread_mutex_t pa_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t pa_lisp_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  pa_lisp_cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t pa_c_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  pa_c_cond = PTHREAD_COND_INITIALIZER;

#define pa_free(v)                                      \
        do {                                            \
                if (v != NULL) {                        \
                        free(v);                        \
                        v = NULL;                       \
                }                                       \
        } while (0)

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

#define RETURN_IF_NULLPTR(v, msg)                       \
        do {                                            \
                if (v == NULL) {                        \
                        pa_error(msg);                  \
                        return 1;                       \
                }                                       \
        } while (0)

/* Continue the audio cycle in Lisp after gc if it is the last cycle. */
#define MAYBE_CONTINUE_IN_LISP(busy, label)                                   \
        do {                                                                  \
                int __busy;                                                   \
                __busy = READ_ONCE(busy);                                     \
                read_memory_barrier();                                        \
                if ((!(__busy)) &&                                            \
                    (!(pa_has_cached_inputs()))) {                            \
                        goto label;                                           \
                }                                                             \
        } while (0)

static void pa_set_error_msg(const char *msg);
static void pa_error(const char *msg);
static int pa_alloc_input_cache(void);
static void pa_free_input_cache(void);
static void pa_process_thread_wait(void);
static void pa_cleanup(void);
static void pa_increment_cycle_counter(char type);
static void *pa_process_thread(void *arg);
static int pa_set_xrun(void);
static PaTime pa_update_latency(PaTime value, PaTime default_value,
                                unsigned long nframes, double srate);

int pa_is_last_cycle(void);
int pa_cache_inputs(void);
int pa_has_cached_inputs(void);
void pa_clear_cached_inputs(void);
int pa_inputs_from_cache_begin(void);
int pa_inputs_from_cache_end(void);
char *pa_get_error_msg(void);
void pa_condition_wait(void);
void pa_set_lisp_busy_state(int status);
void pa_transfer_to_c_thread(void);
int pa_set_thread_callback(int has_cached_inputs);
int pa_get_buffer_size(void);
double pa_get_stream_latency(int is_input);
void pa_set_stream_latency(double value, int is_input);
SAMPLE pa_get_sample_rate(void);
struct pa_xrun *pa_get_xruns(void);
void pa_xrun_reset(void);
int pa_initialize(unsigned int input_channels, unsigned int output_channels,
                  unsigned long nframes, const char* client_name,
                  SAMPLE *sample_counter);
int pa_start(void);
int pa_stop(void);
void pa_set_lisp_io(SAMPLE *input, SAMPLE *output);
unsigned long pa_cycle_begin(void);
void pa_continue_cycle_begin(signed long nframes);
void pa_cycle_end(unsigned long nframes);
SAMPLE pa_get_cycle_start_time(void);
double pa_get_time_offset_seconds(void);
uint32_t pa_get_time_offset_frames(void);
PaStream *pa_stream(void);
void pa_set_devices(PaDeviceIndex input, PaDeviceIndex output);

#endif  /* __RTPA_H */
