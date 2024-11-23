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

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "rtjack.h"

static void ja_set_error_msg(const char *msg)
{
	strncpy(ja_error_msg, msg, JA_ERROR_MSG_MAX_LENGTH - 1);
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

static int ja_alloc_input_cache(void)
{
	unsigned int i, size;

	/* Cached inputs + cycle counter. */
	ja_inputs_cache = (struct incudine_ringbuffer **)
		malloc((ja_in_channels + 1) * sizeof(struct incudine_ringbuffer*));
	RETURN_IF_NULLPTR(ja_inputs_cache, "malloc failure");

	size = (unsigned int) (ja_sample_rate * MAX_GC_TIME_SEC * JA_SAMPLE_SIZE);
	for (i = 0; i < ja_in_channels; i++) {
		ja_inputs_cache[i] = incudine_ringbuffer_create(size);
		RETURN_IF_NULLPTR(ja_inputs_cache[i],
		                  "incudine_ringbuffer_create() failure");
	}
	/* Cycle counter: 1 byte = 1 cycle. */
	ja_inputs_cache[ja_in_channels] =
		incudine_ringbuffer_create(JA_MAX_RECOVERED_CYCLES);
	RETURN_IF_NULLPTR(ja_inputs_cache[ja_in_channels],
	                  "incudine_ringbuffer_create() failure");
	return 0;
}

static void ja_free_input_cache(void)
{
	int i;

	if (ja_inputs_cache != NULL) {
		for (i = 0; i <= ja_in_channels; i++) {
			if (ja_inputs_cache[i] != NULL)
				incudine_ringbuffer_free(ja_inputs_cache[i]);
		}
		free(ja_inputs_cache);
		ja_inputs_cache = NULL;
	}
}

static void ja_increment_cycle_counter(char type)
{
	if (ja_inputs_cache != NULL) {
		/*
		 * The buffer value informs if the cycle is the last.
		 * type is JA_SUSPENDED_CYCLE or JA_CONTINUE_LAST_CYCLE.
		 */
		incudine_ringbuffer_write(ja_inputs_cache[ja_in_channels], &type, 1);
	}
}

int ja_is_last_cycle(void)
{
	if (ja_has_cached_inputs()) {
		int n;
		char c;

		n = incudine_ringbuffer_peek(ja_inputs_cache[ja_in_channels], &c, 1);
		if ((n == 1) && (c == JA_CONTINUE_LAST_CYCLE))
			return 1;
	}
	return 0;
}

int ja_cache_inputs(void)
{
	int i, n, res;

	res = 0;
	if (ja_inputs_cache != NULL) {
		for (i = 0; i < ja_in_channels; i++) {
			n = incudine_ringbuffer_write(ja_inputs_cache[i], (char *) ja_inputs[i],
			                              ja_buffer_bytes);
			res += (n > 0);
		}
		res = (res == ja_in_channels);
		ja_increment_cycle_counter(JA_SUSPENDED_CYCLE);
	}
	return res;
}

int ja_has_cached_inputs(void)
{
	int n = 1;

	if (ja_inputs_cache != NULL)
		n = incudine_ringbuffer_empty(ja_inputs_cache[ja_in_channels]);
	return (n == 0);
}

void ja_clear_cached_inputs(void)
{
	int i;

	if (ja_inputs_cache != NULL) {
		for (i = 0; i <= ja_in_channels; i++)
			incudine_ringbuffer_reset(ja_inputs_cache[i]);
	}
}

/*
 * Retrieve the cached inputs.
 * The cycle counter is not updated.
*/
int ja_inputs_from_cache_begin(void)
{
	int i, n, res;

	res = 0;
	if (ja_inputs_cache != NULL) {
		for (i = 0; i < ja_in_channels; i++) {
			SAMPLE *tmp;
			int j;

			n = incudine_ringbuffer_read(ja_inputs_cache[i], (char *) ja_tmp_inputs,
			                             ja_buffer_bytes);
			res |= (n > 0);
			tmp = lisp_input + i;
			for (j = 0; j < ja_frames; j++) {
				*tmp = (SAMPLE) ja_tmp_inputs[j];
				tmp += ja_in_channels;
			}
		}
	}
	return res;
}

/* Update the cycle counter after ja_inputs_from_cache_begin(). */
int ja_inputs_from_cache_end(void)
{
	int res = 0;

	if (ja_inputs_cache != NULL) {
		char c;
		res = incudine_ringbuffer_read(ja_inputs_cache[ja_in_channels], &c, 1);
	}
	return res;
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

	if (ja_in_channels > 0) {
		RETURN_IF_NULLPTR(input_port_names, "missing input port names");
		input_ports =
			(jack_port_t **) malloc(sizeof(jack_port_t *) * ja_in_channels);
		RETURN_IF_NULLPTR(input_ports, "malloc failure");
		for (i = 0; i < ja_in_channels; i++) {
			input_ports[i] =
				jack_port_register(client, input_port_names[i],
				                   JACK_DEFAULT_AUDIO_TYPE,
				                   JackPortIsInput, 0);
		}
	}
	if (ja_out_channels > 0) {
		RETURN_IF_NULLPTR(output_port_names, "missing output port names");
		output_ports =
			(jack_port_t **) malloc(sizeof(jack_port_t *) * ja_out_channels);
		RETURN_IF_NULLPTR(output_ports, "malloc failure");
		for (i = 0; i < ja_out_channels; i++) {
			output_ports[i] =
				jack_port_register (client, output_port_names[i],
				                    JACK_DEFAULT_AUDIO_TYPE,
				                    JackPortIsOutput, 0);
		}
	}
	return 0;
}

void set_port_names(char **input, char **output)
{
	input_port_names = input;
	output_port_names = output;
}

const char *ja_port_name(int direction, unsigned int number)
{
	jack_port_t **ports = NULL;
	unsigned int channels;

	if (direction == JA_INPUT_PORT) {
		ports = input_ports;
		channels = ja_in_channels;
	} else if (direction == JA_OUTPUT_PORT) {
		ports = output_ports;
		channels = ja_out_channels;
	}
	if (ports != NULL && number < channels)
		return jack_port_name(ports[number]);
	return NULL;
}

int ja_set_port_name(int direction, unsigned int number, const char *name)
{
	if (client != NULL) {
		jack_port_t **ports = NULL;
		unsigned int channels;

		if (direction == JA_INPUT_PORT) {
			ports = input_ports;
			channels = ja_in_channels;
		} else if (direction == JA_OUTPUT_PORT) {
			ports = output_ports;
			channels = ja_out_channels;
		}
		if (ports != NULL && number < channels)
			return jack_port_rename(client, ports[number], name);
	}
	return -1;
}

static void ja_connect_client(void)
{
	char **ports;
	int i;

	ports = (char **) jack_get_ports(client, NULL, NULL,
	                                 JackPortIsPhysical | JackPortIsOutput);
	if (ports != NULL) {
		for (i = 0; i < ja_in_channels && ports[i] != NULL; i++)
			jack_connect(client, ports[i], jack_port_name(input_ports[i]));
		jack_free(ports);
	}
	ports = (char **) jack_get_ports(client, NULL, NULL,
	                                 JackPortIsPhysical | JackPortIsInput);
	if (ports == NULL) return;

	for (i = 0; i < ja_out_channels && ports[i] != NULL; i++)
		jack_connect(client, jack_port_name(output_ports[i]), ports[i]);
	jack_free(ports);
}

static void* ja_process_thread(void *arg)
{
	(void) arg;

	ja_proc_thread.id = pthread_self();
	ja_proc_thread.status = JA_RUNNING;

	while (ja_status == JA_RUNNING) {
		int lisp_busy;

		lisp_busy = ja_lisp_busy;
		read_memory_barrier();
		if (lisp_busy) {
			int i;
			jack_nframes_t frames;

			frames = jack_cycle_wait(client);
			if (frames > lisp_max_bufsize)
				frames = lisp_max_bufsize;
			if (ja_frames != frames) {
				/* Buffer size is changed */
				ja_frames = frames;
				ja_buffer_bytes = frames * JA_SAMPLE_SIZE;
			}
			for (i = 0; i < ja_out_channels; i++) {
				ja_outputs[i] = jack_port_get_buffer(output_ports[i], ja_frames);
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
			 * cycle and ONLY ONE TIME after gc in SBCL. The rt lisp
			 * thread uses `jack_cycle_wait' and `jack_cycle_signal'
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
	ja_proc_thread.status = JA_STOPPED;
	return 0;
}

static void* ja_process_thread_with_cached_inputs(void *arg)
{
	unsigned int cycle_time_offset = 0;
	(void) arg;

	ja_proc_thread.id = pthread_self();
	ja_proc_thread.status = JA_RUNNING;

	while (ja_status == JA_RUNNING) {
		int lisp_busy;

		lisp_busy = ja_lisp_busy;
		read_memory_barrier();
		if (lisp_busy) {
			int i;
			jack_nframes_t frames;

			frames = jack_cycle_wait(client);
			if (frames > lisp_max_bufsize)
				frames = lisp_max_bufsize;
			if (ja_frames != frames) {
				ja_frames = frames;
				ja_buffer_bytes = frames * JA_SAMPLE_SIZE;
				ja_free(ja_tmp_inputs);
				if (ja_in_channels > 0) {
					ja_tmp_inputs = (jack_default_audio_sample_t *)
					                  malloc(ja_buffer_bytes);
					if (ja_tmp_inputs == NULL) {
						ja_proc_thread.status = JA_STOPPED;
						ja_error("malloc failure");
						return 0;
					}
				}
				if (ja_inputs_cache != NULL)
					ja_clear_cached_inputs();
			}
			/* Store JACK MIDI inputs. */
			jm_process2(frames, JM_CACHED_INPUTS, cycle_time_offset);
			/*
			 * ja_cycle_start_time will be updated from Lisp
			 * during the last cycle.
			 */
			cycle_time_offset += frames;
			/*
			 * The audio cycle could continue in Lisp after gc
			 * if there are not other cycles to recover.
			 */
			MAYBE_CONTINUE_IN_LISP(ja_lisp_busy, resume_lisp);

			/* Cached inputs used if it is not the last cycle. */
			if (ja_inputs_cache != NULL) {
				for (i = 0; i < ja_in_channels; i++) {
					ja_inputs[i] = jack_port_get_buffer(input_ports[i], ja_frames);
					incudine_ringbuffer_write(ja_inputs_cache[i],
					                          (char *) ja_inputs[i],
					                          ja_buffer_bytes);
				}
			}
			MAYBE_CONTINUE_IN_LISP(ja_lisp_busy, resume_lisp);

			/*
			 * The outputs are not zero if it is the last cycle,
			 * because it ends in Lisp.
			 */
			for (i = 0; i < ja_out_channels; i++) {
				ja_outputs[i] = jack_port_get_buffer(output_ports[i], ja_frames);
				memset(ja_outputs[i], 0, ja_buffer_bytes);
			}
			MAYBE_CONTINUE_IN_LISP(ja_lisp_busy, resume_lisp);

			/* This cycle is not the last; it is to recover. */
			jack_cycle_signal(client, 0);
			ja_increment_cycle_counter(JA_SUSPENDED_CYCLE);
		} else {
		resume_lisp:
			/*
			 * Continue the last cycle in Lisp-thread and block
			 * the current C-thread.
			 */
			__ja_condition_signal(&ja_lisp_cond, &ja_lisp_lock);
			__ja_condition_wait(&ja_c_cond, &ja_c_lock);

			cycle_time_offset = 0;
			ja_cycle_start_time = *ja_sample_counter;
		}
	}
	ja_proc_thread.status = JA_STOPPED;
	return 0;
}

static int ja_xrun_cb(void *arg)
{
	ja_xruns.count++;
	ja_xruns.last_time = *((SAMPLE *) arg);
	return 0;
}

struct ja_xrun *ja_get_xruns(void)
{
	return &ja_xruns;
}

void ja_xrun_reset(void)
{
	ja_xruns.count = 0;
	ja_xruns.last_time = (SAMPLE) 0.0;
}

static void ja_shutdown(void *arg)
{
	(void) arg;

	ja_status = JA_SHUTDOWN;
	fprintf(stderr, "JACK Audio shutdown\n");
	kill(getpid(), SIGQUIT);
}

static int ja_set_buffer_size_cb(jack_nframes_t nframes, void *arg)
{
	(void) arg;

	if (nframes > lisp_max_bufsize) {
		fprintf(stderr, "WARNING: the JACK buffer size is set to %d "
			"but the maximum size\n         for Incudine is %d. "
			"See INCUDINE:SET-MAX-BUFFER-SIZE.\n",
			nframes, lisp_max_bufsize);
	} else {
		int lisp_busy;
		lisp_busy = ja_lisp_busy;
		read_memory_barrier();
		if (!lisp_busy && !ja_has_cached_inputs())
			ja_frames = nframes;
		fprintf(stderr, "The JACK buffer size is set to %d.\n"
			"The maximum size for Incudine is %d.\n",
			nframes, lisp_max_bufsize);
	}
	return 0;
}

static void ja_process_thread_wait(void)
{
	struct timespec time;
	int i, ret;

	time.tv_sec = 0;
	time.tv_nsec = 1000000;
	/* Maximum 3 seconds. */
	for (i = 0; i < 3000; i++) {
		nanosleep(&time, NULL);
		if (ja_proc_thread.status == JA_STOPPED) return;
	}
	__ja_condition_signal(&ja_c_cond, &ja_c_lock);
	/* Maximum 2 seconds. */
	for (i = 0; i < 2000; i++) {
		nanosleep(&time, NULL);
		if (ja_proc_thread.status == JA_STOPPED) return;
	}
	fprintf(stderr, "Detaching the foreign process thread.\n");
	ret = pthread_detach(ja_proc_thread.id);
	if (ret != 0 && ja_proc_thread.status != JA_STOPPED) {
		fprintf(stderr, "Cannot detach the foreign process thread.\n");
		ja_shutdown(NULL);
	}
}

static void ja_cleanup(void *arg)
{
	(void) arg;

	if (ja_status == JA_STOPPED) return;
	pthread_mutex_lock(&ja_lock);
	ja_status = JA_STOPPING;
	if (ja_proc_thread.status != JA_STOPPED)
		ja_process_thread_wait();
	if (client != NULL) {
		jack_deactivate(client);
		jack_client_close(client);
		client = NULL;
	}
	ja_free(ja_inputs);
	ja_free(ja_tmp_inputs);
	ja_free(ja_outputs);
	ja_free_input_cache();
	ja_free(input_ports);
	ja_free(output_ports);
	ja_free(jm_pad_buffer);
	ja_free(jm_inputs);
	ja_free(jm_outputs);
	ja_free(jm_invec_tmp.data);
	ja_free(jm_outvec_tmp.data);
	ja_in_channels = 0;
	ja_out_channels = 0;
	ja_sample_counter = NULL;
	ja_cycle_start_time = (SAMPLE) 0.0;
	ja_status = JA_STOPPED;
	pthread_mutex_unlock(&ja_lock);
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
	write_memory_barrier();
	ja_lisp_busy = status;
}

/* Transfer the control of the client to C realtime thread */
void ja_transfer_to_c_thread(void)
{
	write_memory_barrier();
	ja_lisp_busy = TRUE;
	__ja_condition_signal(&ja_c_cond, &ja_c_lock);
}

int ja_set_thread_callback(int has_cached_inputs)
{
	ja_thread_callback = (has_cached_inputs ?
	                      ja_process_thread_with_cached_inputs
	                      : ja_process_thread);
	return has_cached_inputs;
}

unsigned int ja_get_buffer_size(void)
{
	return ja_frames;
}

SAMPLE ja_get_sample_rate(void)
{
	return ja_sample_rate;
}

int ja_initialize(unsigned int input_channels, unsigned int output_channels,
                  unsigned int nframes, const char* client_name,
                  SAMPLE *sample_counter)
{
	sigset_t sset;
	int err;
	(void) nframes;

	ja_cleanup(NULL);
	ja_error_msg[0] = '\0';
	client = jack_client_open(client_name, JackNoStartServer, NULL);
	if (client == NULL) {
		ja_set_error_msg("jack_client_open failure");
		return 1;
	}
	ja_proc_thread.status = JA_STOPPED;
	ja_status = JA_INITIALIZING;
	ja_frames = jack_get_buffer_size(client);
	if (ja_frames > lisp_max_bufsize)
		ja_frames = lisp_max_bufsize;
	ja_sample_rate = (SAMPLE) jack_get_sample_rate(client);
	ja_sample_duration = 1.0 / ja_sample_rate;
	ja_in_channels = input_channels;
	ja_out_channels = output_channels;
	ja_buffer_bytes = ja_frames * JA_SAMPLE_SIZE;
	ja_cycle_start_time = *sample_counter;
	ja_sample_counter = sample_counter;

	if (input_channels > 0) {
		ja_inputs = (jack_default_audio_sample_t **)
			malloc(sizeof(jack_default_audio_sample_t *) * input_channels);
		RETURN_IF_NULLPTR(ja_inputs, "malloc failure");

		ja_tmp_inputs = (jack_default_audio_sample_t *) malloc(ja_buffer_bytes);
		RETURN_IF_NULLPTR(ja_tmp_inputs, "malloc failure");
	}
	err = ja_alloc_input_cache();
	/* RETURN_IF_NULLPTR called from ja_alloc_input_cache() */
	if (err) return err;

	if (output_channels > 0) {
		ja_outputs = (jack_default_audio_sample_t **)
			malloc(sizeof(jack_default_audio_sample_t *) * output_channels);
		RETURN_IF_NULLPTR(ja_outputs, "malloc failure");
	}
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

	if (ja_thread_callback == NULL)
		ja_set_thread_callback(JA_THREAD_CALLBACK_DEFAULT);
	jack_set_process_thread(client, ja_thread_callback, NULL);
	jack_on_shutdown(client, ja_shutdown, NULL);
	jack_set_xrun_callback(client, ja_xrun_cb, (void *) sample_counter);
	jack_set_buffer_size_callback(client, ja_set_buffer_size_cb, NULL);
	ja_xrun_reset();

	/* Unblock signals */
	sigemptyset(&sset);
	if (sigprocmask(SIG_SETMASK, &sset, NULL) < 0) {
		ja_error("Unblock signals error\n");
		return 1;
	}
	sigemptyset(&sig_stop_for_gc);
	sigaddset(&sig_stop_for_gc, SBCL_SIG_STOP_FOR_GC);

	ja_lisp_busy = TRUE;

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
	ja_cleanup(NULL);
	return 0;
}

void ja_set_lisp_io(SAMPLE *input, SAMPLE *output)
{
	lisp_input = input;
	lisp_output = output;
}

void ja_set_lisp_max_bufsize(unsigned int value)
{
	lisp_max_bufsize = value;
	lisp_bufsize_mask = value - 1;
}

jack_nframes_t ja_cycle_begin(void)
{
	int i;
	jack_nframes_t frames;

	if (ja_status != JA_RUNNING)
		return 0;
	/*
	 * We are calling `ja_cycle_begin' from lisp, and the signal
	 * sent by SBCL during the gc (SIGUSR2) interrupts `sem_timedwait'
	 * used by Jack (i.e. JACK2 shuts down the client thread).
	 */
	pthread_sigmask(SIG_BLOCK, &sig_stop_for_gc, NULL);
	frames = jack_cycle_wait(client);
	frames = 1 + ((frames - 1) & lisp_bufsize_mask);
	pthread_sigmask(SIG_UNBLOCK, &sig_stop_for_gc, NULL);

	if (ja_status != JA_RUNNING)
		return 0;

	for (i = 0; i < ja_in_channels; i++) {
		SAMPLE *tmp;
		int j;

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

/* Called from Lisp to continue the last cycle started from C-thread. */
void ja_continue_cycle_begin(jack_nframes_t frames)
{
	int i;

	for (i = 0; i < ja_in_channels; i++) {
		SAMPLE *tmp;
		int j;

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

double ja_get_time_offset_seconds(void)
{
	if (client != NULL) {
		jack_nframes_t jack_frames;
		int frames;

		jack_frames = jack_frames_since_cycle_start(client);
		frames = (int) (*ja_sample_counter - ja_cycle_start_time);
		if (frames > jack_frames)
			return (frames - jack_frames) * ja_sample_duration;
	}
	return 0.0;
}

jack_nframes_t ja_get_time_offset_frames(void)
{
	if (client != NULL) {
		jack_nframes_t jack_frames;
		int frames;

		jack_frames = jack_frames_since_cycle_start(client);
		frames = (int) (*ja_sample_counter - ja_cycle_start_time);
		if (frames > jack_frames)
			return (jack_nframes_t) (frames - jack_frames);
	}
	return 0;
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
		p->rb = incudine_ringbuffer_create(JM_RINGBUFFER_SIZE);
		if (p->rb != NULL) {
			p->cache = incudine_ringbuffer_create(JM_RINGBUFFER_SIZE);
			if (p->cache == NULL) {
				incudine_ringbuffer_free(p->rb);
				p->rb = NULL;
			}
		}
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
				incudine_ringbuffer_free(q->rb);
				incudine_ringbuffer_free(q->cache);
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
			incudine_ringbuffer_free(p->rb);
			p->rb = NULL;
		}
		if (p->cache != NULL) {
			incudine_ringbuffer_free(p->cache);
			p->cache = NULL;
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

	vec->data = (struct jm_data **) malloc(tmp->size * sizeof(struct jm_data *));
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
			realloc(vec->data, vec->size * sizeof(struct jm_data *));
		if (vec->data == NULL)
			return JM_MEMORY_ERROR;
	}
	for (q = vec->data; *q != NULL; q++) {
		if (*q == p)
			return 0;
	}
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
			if (vec->count != 0)
				vec->count--;
			else
				fprintf(stderr, "Error: inconsistent Jack MIDI data\n");
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

static void jm_process2(jack_nframes_t frames, int has_cached_inputs,
                        unsigned int time_offset)
{
	struct jm_data **in, **out;
	struct incudine_ringbuffer *rb;

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
			rb = (has_cached_inputs ? p->cache : p->rb);
			if (incudine_ringbuffer_write_space(rb) >= size) {
				double *time;
				uint32_t *len;
				time = (double *) jm_pad_buffer;
				*time = (double) (ja_cycle_start_time + ev.time);
				if (has_cached_inputs)
					*time += time_offset;
				len = (uint32_t *) (time + 1);
				*len = ev.size;
				memcpy(len + 1, ev.buffer, ev.size);
				incudine_ringbuffer_write(rb, jm_pad_buffer, size);
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
	if (has_cached_inputs)
		return;
	for (out = jm_outputs; *out != NULL; out++) {
		(*out)->port_buffer = jack_port_get_buffer((*out)->port, frames);
		jack_midi_clear_buffer((*out)->port_buffer);
	}
}

void jm_process(jack_nframes_t frames)
{
	jm_process2(frames, 0, 0);
}

/* Write data_size bytes of a MIDI message encoded into four bytes. */
int jm_write_short(struct jm_data *p, uint32_t msg, unsigned int data_size)
{
	jack_midi_data_t *buf;
	jack_nframes_t time;

	if (p == NULL || p->port_buffer == NULL)
		return JM_WRITE_ERROR;
	if (data_size > 4)
		return JM_DATASIZE_ERROR;

	/*
	 * The variable ja_lisp_busy is:
	 *   - always FALSE from Lisp-thread
	 *   - TRUE from C-thread if the audio cycle is not the last
	 *   - TRUE or FALSE from C-thread if the audio cycle is the last
	 */
	if (ja_lisp_busy) {
		int n;
		/* Events cached during recovery of suspended cycles. */
		n = incudine_ringbuffer_write(p->cache, (char *) &msg, data_size);
		if (n == data_size)
			return 0;
	} else {
		time = (jack_nframes_t) (*ja_sample_counter - ja_cycle_start_time);
		buf = jack_midi_event_reserve(p->port_buffer, time, data_size);
		if (buf != NULL) {
			memcpy(buf, &msg, data_size);
			return 0;
		}
	}
	return JM_WRITE_ERROR;
}

/* Write data_size bytes of a MIDI message stored into a buffer. */
int jm_write(struct jm_data *p, char *buffer, unsigned int data_size)
{
	jack_midi_data_t *jbuf;
	jack_nframes_t time;

	if (p == NULL || p->port_buffer == NULL)
		return JM_WRITE_ERROR;

	/*
	 * The variable ja_lisp_busy is:
	 *   - always FALSE from Lisp-thread
	 *   - TRUE from C-thread if the audio cycle is not the last
	 *   - TRUE or FALSE from C-thread if the audio cycle is the last
	 */
	if (ja_lisp_busy) {
		int n;
		/* Events cached during recovery of suspended cycles. */
		n = incudine_ringbuffer_write(p->cache, buffer, data_size);
		if (n == data_size)
			return 0;
	} else {
		time = (jack_nframes_t) (*ja_sample_counter - ja_cycle_start_time);
		jbuf = jack_midi_event_reserve(p->port_buffer, time, data_size);
		if (jbuf != NULL) {
			memcpy(jbuf, buffer, data_size);
			return 0;
		}
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
	n = incudine_ringbuffer_read_space(p->rb);
	if (n > 0)
		memset(b, 0, bufsize);
	while (n > 0 && remain > JM_HEADER_SIZE) {
		ret = incudine_ringbuffer_peek(p->rb, (char *) b, JM_HEADER_SIZE);
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
			ret = incudine_ringbuffer_read(p->rb, b, ev_size);
			ev_size += pad;
			b += ev_size;
			remain -= ev_size;
			count++;
		} else if (ev_size > bufsize) {
			/* Ignore the event. */
			incudine_ringbuffer_read_advance(p->rb, ev_size);
		} else {
			/* The buffer is full. */
			break;
		}
		n = incudine_ringbuffer_read_space(p->rb);
	}
	p->remain = n;
	return count;
}

void jm_flush_pending(struct jm_data *p)
{
	if (p != NULL)
		incudine_ringbuffer_reset(p->rb);
}

void jm_waiting_for(struct jm_input_data *p)
{
	if (p != NULL && p->remain == 0) {
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

void jm_clear_cached_events(void)
{
	struct jm_data **p;

	for (p = jm_inputs; *p != NULL; p++) {
		if ((*p)->cache != NULL)
			incudine_ringbuffer_reset((*p)->cache);
	}
	for (p = jm_outputs; *p != NULL; p++) {
		if ((*p)->cache != NULL)
			incudine_ringbuffer_reset((*p)->cache);
	}
}

static int jm_write_cached_midi_output(struct jm_data *p)
{
	jack_midi_data_t *jbuf;
	unsigned int data_size;

	data_size = incudine_ringbuffer_read_space(p->cache);
	if (data_size == 0)
		return 0;
	jbuf = jack_midi_event_reserve(p->port_buffer, 0, data_size);
	if (jbuf != NULL) {
		int n;

		n = incudine_ringbuffer_read(p->cache, (char *) jbuf, data_size);
		if (n == data_size)
			return 0;
	}
	return JM_WRITE_ERROR;
}

int jm_write_cached_midi_outputs(void)
{
	struct jm_data **p;
	int res;

	res = 0;
	for (p = jm_outputs; *p != NULL; p++) {
		if ((*p)->port_buffer == NULL) {
			res = JM_WRITE_ERROR;
		} else {
			res |= jm_write_cached_midi_output(*p);
		}
	}
	return res;
}

static unsigned int jm_next_midi_event_position(char *buffer,
                                                unsigned int bufsize)
{
	uint32_t len;

	len = ((uint32_t *) buffer)[2];
	if (len >= bufsize)
		return 0;
	return JM_HEADER_SIZE + len;
}

static int jm_cached_midi_inputs_read_space(struct incudine_ringbuffer *rb,
                                            jack_nframes_t frames)
{
	char *b;
	int n, size, bufsize;

	n = incudine_ringbuffer_read_space(rb);
	if (n == 0)
		return 0;
	bufsize = incudine_ringbuffer_peek(rb, jm_pad_buffer, n);
	if (bufsize != n)
		return 0;
	b = jm_pad_buffer;
	size = -1;
	for (n = 0; n < bufsize; n += size) {
		double time;

		time = *((double *) b) - *ja_sample_counter;
		if (time >= frames)
			return n;
		size = jm_next_midi_event_position(b, bufsize);
		if (size == 0)
			return n;
		bufsize -= size;
		if (bufsize <= 0)
			return n;
		b += size;
	}
	return n;
}

int jm_read_cached_midi_inputs(jack_nframes_t frames)
{
	struct jm_data **in;
	int res;

	res = 0;
	for (in = jm_inputs; *in != NULL; in++) {
		struct jm_input_data *p;
		int n, m;

		p = (struct jm_input_data *) *in;
		p->port_buffer = jack_port_get_buffer(p->port, frames);
		if (p->port_buffer == NULL)
			continue;
		n = jm_cached_midi_inputs_read_space(p->cache, frames);
		if (n > 0) {
			m = incudine_ringbuffer_write_space(p->rb);
			if (n <= m) {
				m = incudine_ringbuffer_read(p->cache, jm_pad_buffer, n);
				n = incudine_ringbuffer_write(p->rb, jm_pad_buffer, n);
				if (n != m)
					res = JM_WRITE_ERROR;
			}
		}
	}
	return res;
}
