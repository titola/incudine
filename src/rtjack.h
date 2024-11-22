/*
 * Copyright (c) 2013-2024 Tito Latini
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

#ifndef __RTJACK_H
#define __RTJACK_H

#include <signal.h>
#include <pthread.h>
#include <time.h>
#include <jack/jack.h>
#include <jack/midiport.h>
#include "barrier.h"
#include "common.h"
#include "ringbuffer.h"

enum {
	JA_RUNNING,
	JA_STOPPED,
	JA_SHUTDOWN,
	JA_INITIALIZING,
	JA_STOPPING
};

enum {
	JA_INPUT_PORT,
	JA_OUTPUT_PORT
};

#define SBCL_SIG_STOP_FOR_GC  SIGUSR2
#define MAX_GC_TIME_SEC  0.33

#define JA_ERROR_MSG_MAX_LENGTH  256
#define JA_PORT_NAME_MAX_LENGTH  16
#define JA_SAMPLE_SIZE  (sizeof(jack_default_audio_sample_t))
#define JA_RESYNC_TIME_WINDOW_NSEC  300000
#define JA_MAX_RECOVERED_CYCLES  32

#define JA_SUSPENDED_CYCLE      0
#define JA_CONTINUE_LAST_CYCLE  1

#define JA_THREAD_CALLBACK_DEFAULT  0

struct ja_xrun {
	unsigned int count;
	SAMPLE last_time;
};

struct ja_thread {
	pthread_t id;
	int status;
};

static jack_client_t *client = NULL;
static SAMPLE ja_sample_rate, ja_sample_duration;
static SAMPLE *ja_sample_counter;
static SAMPLE ja_cycle_start_time; /* Cycle start time in samples. */
static unsigned int ja_in_channels, ja_out_channels, ja_frames;
static size_t ja_buffer_bytes;
static int ja_status = JA_STOPPED;
static int ja_lisp_busy;
static unsigned int lisp_max_bufsize, lisp_bufsize_mask;
static SAMPLE *lisp_input, *lisp_output;
static jack_default_audio_sample_t **ja_inputs, **ja_outputs, *ja_tmp_inputs;
static jack_port_t **input_ports, **output_ports;
static char **input_port_names, **output_port_names;
static struct incudine_ringbuffer **ja_inputs_cache = NULL;
static JackThreadCallback ja_thread_callback = NULL;
static struct ja_thread ja_proc_thread;
static pthread_mutex_t ja_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t ja_lisp_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  ja_lisp_cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t ja_c_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  ja_c_cond = PTHREAD_COND_INITIALIZER;
static struct ja_xrun ja_xruns;
static char ja_error_msg[JA_ERROR_MSG_MAX_LENGTH];
static sigset_t sig_stop_for_gc;

#define RETURN_IF_NULLPTR(v, msg)                       \
	do {                                            \
		if (v == NULL) {                        \
			ja_error(msg);                  \
			return 1;                       \
		}                                       \
	} while (0)

#define ja_free(v)                                      \
	do {                                            \
		if (v != NULL) {                        \
			free(v);                        \
			v = NULL;                       \
		}                                       \
	} while (0)

#define __ja_condition_wait(c, l)                       \
	do {                                            \
		pthread_mutex_lock(l);                  \
		pthread_cond_wait(c, l);                \
		pthread_mutex_unlock(l);                \
	} while (0)

#define __ja_condition_signal(c, l)                     \
	do {                                            \
		pthread_mutex_lock(l);                  \
		pthread_cond_signal(c);                 \
		pthread_mutex_unlock(l);                \
	} while (0)

/* Continue the audio cycle in Lisp after gc if it is the last cycle. */
#define MAYBE_CONTINUE_IN_LISP(busy, label)                                   \
	do {                                                                  \
		int __busy;                                                   \
		__busy = READ_ONCE(busy);                                     \
		read_memory_barrier();                                        \
		if ((!(__busy)) &&                                            \
		    (!(ja_has_cached_inputs()))) {                            \
			ja_increment_cycle_counter(JA_CONTINUE_LAST_CYCLE);   \
			goto label;                                           \
		}                                                             \
	} while (0)

static void ja_set_error_msg(const char *msg);
static void ja_error(const char *msg);
static void ja_default_error_callback(const char *msg);
static void ja_silent_error_callback(const char *msg);
static int ja_alloc_input_cache(void);
static void ja_free_input_cache(void);
static void ja_increment_cycle_counter(char type);
static int ja_register_ports(void);
static void ja_connect_client(void);
static void *ja_process_thread(void *arg);
static void* ja_process_thread_with_cached_inputs(void *arg);
static int ja_xrun_cb(void *arg);
static void ja_shutdown(void *arg);
static void ja_process_thread_wait(void);
static void ja_cleanup(void *arg);

int ja_cache_inputs(void);
int ja_has_cached_inputs(void);
int ja_is_last_cycle(void);
void ja_clear_cached_inputs(void);
int ja_inputs_from_cache_begin(void);
int ja_inputs_from_cache_end(void);
void ja_silent_errors(int silent);
char *ja_get_error_msg(void);
void ja_condition_wait(void);
void ja_set_lisp_busy_state(int status);
void ja_transfer_to_c_thread(void);
int ja_set_thread_callback(int has_cached_inputs);
unsigned int ja_get_buffer_size(void);
SAMPLE ja_get_sample_rate(void);
struct ja_xrun *ja_get_xruns(void);
void ja_xrun_reset(void);
int ja_initialize(unsigned int input_channels, unsigned int output_channels,
                  unsigned int nframes, const char* client_name,
                  SAMPLE *sample_counter);
int ja_start(void);
int ja_stop(void);
void ja_set_lisp_io(SAMPLE *input, SAMPLE *output);
void ja_set_lisp_max_bufsize(unsigned int value);
jack_nframes_t ja_cycle_begin(void);
void ja_continue_cycle_begin(jack_nframes_t frames);
void ja_cycle_end(jack_nframes_t frames);
SAMPLE ja_get_cycle_start_time(void);
double ja_get_time_offset_seconds(void);
jack_nframes_t ja_get_time_offset_frames(void);
jack_client_t *ja_client(void);
void set_port_names(char **input, char **output);
const char *ja_port_name(int direction, unsigned int number);
int ja_set_port_name(int direction, unsigned int number, const char *name);

/* Jack MIDI */

#define JM_INITIAL_MAX_PORTS  18
#define JM_NUMBER_OF_PORTS_INCREMENT  8
#define JM_RINGBUFFER_SIZE  65536
#define JM_HEADER_SIZE  (sizeof(double) + sizeof(uint32_t))
#define JM_CACHED_INPUTS  1

#define JM_MEMORY_ERROR    -1
#define JM_WRITE_ERROR     -2
#define JM_DATASIZE_ERROR  -3

struct jm_data {
	jack_port_t *port;
	void *port_buffer;
	struct incudine_ringbuffer *rb;
	struct incudine_ringbuffer *cache;
};

struct jm_input_data {
	jack_port_t *port;
	void *port_buffer;
	struct incudine_ringbuffer *rb;
	struct incudine_ringbuffer *cache;
	int remain;
	int to_signal;
	pthread_mutex_t lock;
	pthread_cond_t cond;
};

struct jm_data_vec {
	struct jm_data **data;
	unsigned int count;
	unsigned int size;
};

static struct jm_data **jm_inputs, **jm_outputs;
static struct jm_data_vec jm_invec_tmp, jm_outvec_tmp;
static char *jm_pad_buffer;

static void jm_process2(jack_nframes_t frames, int has_cached_inputs,
                        unsigned int time_offset);
static unsigned int jm_next_midi_event_position(char *buffer,
                                                unsigned int bufsize);
static int jm_cached_midi_inputs_read_space(struct incudine_ringbuffer *rb,
                                            jack_nframes_t frames);
static int jm_write_cached_midi_output(struct jm_data *p);

jack_port_t *jm_port_register(const char *port_name, int is_input);
struct jm_data *jm_alloc_data(int is_input);
void jm_free_data(struct jm_data *p, int is_input);
struct jm_data_vec *jm_copy_data_vec(int is_input);
void jm_free_data_vec(struct jm_data_vec *p);
int jm_append_pending_data(struct jm_data *p, int is_input);
void jm_delete_from_pending_data(struct jm_data *p, int is_input);
void jm_update_data(struct jm_data_vec *vec, int is_input);
void jm_process(jack_nframes_t frames);
int jm_write_short(struct jm_data *p, uint32_t msg, unsigned int data_size);
int jm_write(struct jm_data *p, char *buffer, unsigned int data_size);
int jm_read(struct jm_input_data *p, char *buffer, unsigned int bufsize);
void jm_flush_pending(struct jm_data *p);
void jm_waiting_for(struct jm_input_data *p);
void jm_force_cond_signal(struct jm_input_data *p);
void jm_clear_cached_events(void);
int jm_read_cached_midi_inputs(jack_nframes_t frames);
int jm_write_cached_midi_outputs(void);

#endif  /* __RTJACK_H */
