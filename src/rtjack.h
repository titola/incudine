/*
 * Copyright (c) 2013-2014 Tito Latini
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
#include "common.h"

enum {
        JA_RUNNING,
        JA_STOPPED,
        JA_SHUTDOWN
};

#define SBCL_SIG_STOP_FOR_GC  SIGUSR2

#define JA_ERROR_MSG_MAX_LENGTH  (256)
#define JA_PORT_NAME_MAX_LENGTH  (16)
#define JA_SAMPLE_SIZE  (sizeof(jack_default_audio_sample_t))

static jack_client_t *client = NULL;
static SAMPLE ja_sample_rate;
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
static int ja_register_ports(void);
static int ja_connect_client(void);
static void* ja_process_thread(void *arg);
static void ja_shutdown(void *arg);
static void ja_terminate(void *arg);

char *ja_get_error_msg(void);
void ja_condition_wait(void);
void ja_set_lisp_busy_state(int status);
void ja_transfer_to_c_thread(void);
int ja_get_buffer_size(void);
SAMPLE ja_get_sample_rate(void);
int ja_initialize(SAMPLE srate, unsigned int input_channels,
                  unsigned int output_channels, unsigned int nframes,
                  const char* client_name);
int ja_start(void);
int ja_stop(void);
void ja_set_lisp_io(SAMPLE *input, SAMPLE *output);
jack_nframes_t ja_cycle_begin(void);
void ja_cycle_end(jack_nframes_t frames);

#endif  /* __RTJACK_H */
