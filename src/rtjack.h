/*
 * Copyright (c) 2013-2017 Tito Latini
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
#include <jack/jack.h>
#include <jack/midiport.h>
#include <jack/ringbuffer.h>
#include "common.h"

enum {
        JA_RUNNING,
        JA_STOPPED,
        JA_SHUTDOWN
};

#define SBCL_SIG_STOP_FOR_GC  SIGUSR2

#define JA_ERROR_MSG_MAX_LENGTH  256
#define JA_PORT_NAME_MAX_LENGTH  16
#define JA_SAMPLE_SIZE  (sizeof(jack_default_audio_sample_t))

struct ja_xrun {
        unsigned int count;
        SAMPLE last_time;
};

static jack_client_t *client = NULL;
static SAMPLE ja_sample_rate;
static SAMPLE *ja_sample_counter;
static SAMPLE ja_cycle_start_time; /* Cycle start time in samples. */
static unsigned int ja_in_channels, ja_out_channels, ja_frames;
static size_t ja_buffer_bytes;
static int ja_status = JA_STOPPED;
static int ja_lisp_busy;
static SAMPLE *lisp_input, *lisp_output;
static jack_default_audio_sample_t **ja_inputs, **ja_outputs;
static jack_port_t **input_ports, **output_ports;
static char **input_port_names, **output_port_names;
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

static void ja_set_error_msg(const char *msg);
static void ja_error(const char *msg);
static void ja_default_error_callback(const char *msg);
static void ja_silent_error_callback(const char *msg);
static int ja_register_ports(void);
static int ja_connect_client(void);
static void *ja_process_thread(void *arg);
static int ja_xrun_cb(void *arg);
static void ja_shutdown(void *arg);
static void ja_terminate(void *arg);

void ja_silent_errors(int silent);
char *ja_get_error_msg(void);
void ja_condition_wait(void);
void ja_set_lisp_busy_state(int status);
void ja_transfer_to_c_thread(void);
int ja_get_buffer_size(void);
SAMPLE ja_get_sample_rate(void);
struct ja_xrun *ja_get_xruns(void);
void ja_xrun_reset(void);
int ja_initialize(SAMPLE srate, unsigned int input_channels,
                  unsigned int output_channels, unsigned int nframes,
                  const char* client_name, SAMPLE *sample_counter);
int ja_start(void);
int ja_stop(void);
void ja_set_lisp_io(SAMPLE *input, SAMPLE *output);
jack_nframes_t ja_cycle_begin(void);
void ja_cycle_end(jack_nframes_t frames);
SAMPLE ja_get_cycle_start_time(void);
jack_client_t *ja_client(void);

/* Jack MIDI */

#define JM_INITIAL_MAX_PORTS  18
#define JM_NUMBER_OF_PORTS_INCREMENT  8
#define JM_RINGBUFFER_SIZE  65536
#define JM_HEADER_SIZE  (sizeof(double) + sizeof(uint32_t))

#define JM_MEMORY_ERROR    -1
#define JM_WRITE_ERROR     -2
#define JM_DATASIZE_ERROR  -3

struct jm_data {
        jack_port_t *port;
        void *port_buffer;
        jack_ringbuffer_t *rb;
};

struct jm_input_data {
        jack_port_t *port;
        void *port_buffer;
        jack_ringbuffer_t *rb;
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

#endif  /* __RTJACK_H */
