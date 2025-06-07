/*
 * Copyright (c) 2013-2025 Tito Latini
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

#include <fcntl.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>
#include <sndfile.h>
#include <math.h>
#include "common.h"

#ifndef WIN32
#include <signal.h>
#include <unistd.h>
#endif

/* THREAD */

#ifndef WIN32
int pthread_priority(pthread_t thread)
{
	int policy;
	struct sched_param param;

	pthread_getschedparam(thread, &policy, &param);
	return param.sched_priority;
}

int pthread_set_priority(pthread_t thread, int priority)
{
	int policy, minprio;
	struct sched_param param;

	pthread_getschedparam(thread, &policy, &param);
	minprio = sched_get_priority_min(INCUDINE_SCHED_POLICY);
	if (priority < minprio) {
		priority = minprio;
	} else {
		int maxprio = sched_get_priority_max(INCUDINE_SCHED_POLICY);
		if (priority > maxprio)
			priority = maxprio;
	}
	param.sched_priority = priority;
	return pthread_setschedparam(thread, INCUDINE_SCHED_POLICY, &param);
}
#endif

/* MEMORY */

SAMPLE *foreign_alloc_sample(size_t size)
{
	size_t n = size * sizeof(SAMPLE);
	SAMPLE *ptr = (SAMPLE *) malloc(n);

	if (ptr != NULL)
		memset(ptr, 0, n);
	return ptr;
}

void *foreign_zero_sample(SAMPLE *ptr, size_t n)
{
	return memset(ptr, 0, sizeof(SAMPLE) * n);
}

/* TEMPO */

SAMPLE tempo_sync(SAMPLE *now, SAMPLE period)
{
	return *now + period - fmod(*now, period);
}

/* SNDFILE */

#ifdef __INCUDINE_USE_64_BIT_SAMPLE__
#define sf_readf_SAMPLE  sf_readf_double
#else
#define sf_readf_SAMPLE  sf_readf_float
#endif

#define SF_FORMAT_MPEG__  0x230000  /* MPEG-1/2 audio stream (libsndfile-1.1.0 2022-03-27) */

void sndfile_to_buffer(SAMPLE *buf, SNDFILE *sndfile, unsigned long frames,
                       int channels, unsigned long buf_offset, int chunk_frames)
{
	sf_count_t n, incr, remain = frames;
	SAMPLE *p;

	if (remain < chunk_frames)
		chunk_frames = remain;

	incr = chunk_frames * channels;

	for (p = buf + buf_offset * channels; remain > 0; p += incr) {
		if ((n = sf_readf_SAMPLE(sndfile, p, chunk_frames)) == 0) {
			fprintf(stderr, "WARNING: end of file in sndfile_to_buffer() after %ld frames\n"
			                "         instead of the declared %ld frames.\n", frames - remain, frames);
			break;
		}
		remain -= n;
		if (remain < chunk_frames)
			chunk_frames = remain;
	}
}

void map_sndfile_ch_to_buffer(SAMPLE *buf, SNDFILE *sndfile,
                              unsigned long frames, int channels,
                              int buf_channels, unsigned long buf_offset,
                              int chunk_frames, int* chan_map_dest,
                              int* chan_map_src, int chan_map_size)
{
	sf_count_t incr, remain = frames;
	SAMPLE *p, *q;

	if (remain < chunk_frames)
		chunk_frames = remain;

	incr = chunk_frames * channels;
	p = buf + buf_offset * buf_channels;
	q = (SAMPLE *) malloc(incr * sizeof(SAMPLE));
	while (remain > 0) {
		sf_count_t i, n;
		SAMPLE *curr;

		if ((n = sf_readf_SAMPLE(sndfile, q, chunk_frames)) == 0) {
			fprintf(stderr, "sndfile_ch_to_buffer fails (%s)\n",
			        sf_strerror(sndfile));
			break;
		}
		remain -= n;
		for (i = 0, curr = q; i < incr;
		     i += channels, curr += channels, p += buf_channels) {
			sf_count_t k;
			for (k = 0; k < chan_map_size; k++)
				p[chan_map_dest[k]] = curr[chan_map_src[k]];
		}
		if (remain < chunk_frames) {
			chunk_frames = remain;
			incr = remain * channels;
		}
	}
	free(q);
}

struct soundfile_vio_buffer {
	void *data;           /* Pointer to the file data. */
	sf_count_t offset;    /* Read offset location. */
	sf_count_t length;    /* Buffer length. */
	sf_count_t start;     /* Start of sound data (end of header). */
};

static sf_count_t soundfile_vio_get_filelen(void *obj)
{
	struct soundfile_vio_buffer *p = (struct soundfile_vio_buffer *) obj;
	return p->length;
}

static sf_count_t soundfile_vio_seek(sf_count_t offset, int whence, void *obj)
{
	struct soundfile_vio_buffer *p = (struct soundfile_vio_buffer *) obj;
	/* The soundfile package for Incudine requires SEEK-SET. */
	(void) whence;

	p->offset = offset;
	return offset;
}

static sf_count_t soundfile_vio_read(void *buf, sf_count_t count, void *obj)
{
	struct soundfile_vio_buffer *p = (struct soundfile_vio_buffer *) obj;

	if (p->offset + count > p->length) {
		if (p->offset >= p->length)
			return 0;
		count = p->length - p->offset;
	}
	memcpy(buf, p->data + p->offset, count);
	p->offset += count;
	return count;
}

/*
 * soundfile_vio_write() is only used internally.
 */
static sf_count_t soundfile_vio_write(const void *ptr, sf_count_t count, void *obj)
{
	struct soundfile_vio_buffer *p = (struct soundfile_vio_buffer *) obj;

	if (p->offset + count > p->length) {
		if (p->offset >= p->length)
			return 0;
		count = p->length - p->offset;
	}
	memcpy(p->data + p->offset, ptr, count);
	p->offset += count;
	return count;
}

static sf_count_t soundfile_vio_tell(void *obj)
{
	struct soundfile_vio_buffer *p = (struct soundfile_vio_buffer *) obj;
	return p->offset;
}

#define SOUNDFILE_BUFFER_LENGTH  4096  /* int data */

static void soundfile_copy_int(SNDFILE *dest, SNDFILE *src, int channels)
{
	static int data[SOUNDFILE_BUFFER_LENGTH];
	sf_count_t frames, count;

	frames = SOUNDFILE_BUFFER_LENGTH / channels;
	count = frames;
	while (count > 0) {
		count = sf_readf_int(src, data, frames);
		sf_writef_int(dest, data, count);
	}
}

static void soundfile_copy_short(SNDFILE *dest, SNDFILE *src, int channels)
{
	static short data[2 * SOUNDFILE_BUFFER_LENGTH];
	sf_count_t frames, count;

	frames = 2 * SOUNDFILE_BUFFER_LENGTH / channels;
	count = frames;
	while (count > 0) {
		count = sf_readf_short(src, data, frames);
		sf_writef_short(dest, data, count);
	}
}

/*
 * Load the file data into memory (read-only with the libsndfile interface,
 * however the data buffer is directly accessible in Incudine via the
 * SOUNDFILE:FILE-DATA function).
 */
SNDFILE* soundfile_open_virtual(const char *path, SF_INFO *sfinfo,
                                struct soundfile_vio_buffer **obj)
{
	SNDFILE *sf;
	void *data;
	struct soundfile_vio_buffer *p;
	SF_VIRTUAL_IO vio = { soundfile_vio_get_filelen, soundfile_vio_seek,
	                      soundfile_vio_read, soundfile_vio_write,
	                      soundfile_vio_tell };
	SF_INFO info;
	sf_count_t length, more_length, start;
	int fd, bytes, lossless, ret;

	fd = open(path, O_RDONLY);
	if (fd < 0) {
		fprintf(stderr, "ERROR: cannot open \"%s\".\n", path);
		return NULL;
	}
	if ((sfinfo->format & SF_FORMAT_TYPEMASK) == SF_FORMAT_RAW)
		memcpy(&info, sfinfo, sizeof(info));
	else memset(&info, 0, sizeof(info));
	sf = sf_open_fd(fd, SFM_READ, &info, SF_TRUE);
	start = lseek(fd, 0, SEEK_CUR);
	length = lseek(fd, 0, SEEK_END);
	if (length <= start) {
		fprintf(stderr, "ERROR: sound file \"%s\" with no data.\n", path);
		sf_close(sf);
		return NULL;
	}
	bytes = 0;
	lossless = FALSE;
	switch (info.format & SF_FORMAT_TYPEMASK) {
	case SF_FORMAT_FLAC:
		if ((info.format & SF_FORMAT_SUBMASK) == SF_FORMAT_PCM_16) bytes = 2;
		if ((info.format & SF_FORMAT_SUBMASK) == SF_FORMAT_PCM_24) bytes = 3;
		lossless = TRUE;
		break;
	case SF_FORMAT_OGG:        /* Vorbis and Opus encoding. */
	case SF_FORMAT_MPEG__:     /* MP3 encoding etc. */
		bytes = 2;
		break;
	}
	if (bytes) {
		length = info.frames * info.channels * bytes;
		info.format &= ~SF_FORMAT_TYPEMASK;
		info.format |= SF_FORMAT_RAW;
		if (!lossless) {
			info.format &= ~SF_FORMAT_SUBMASK;
			info.format |= SF_FORMAT_PCM_16;
		}
	}
	/* 64-bit aligned plus 8 additional bytes. */
	more_length = length + 7 + 8 - ((length + 7) & 7);
	data = malloc(more_length);
	if (data == NULL) {
		fprintf(stderr, "ERROR: malloc(%ld) failed.\n", more_length);
		sf_close(sf);
		return NULL;
	}
	p = (struct soundfile_vio_buffer *)
		malloc(sizeof(struct soundfile_vio_buffer));
	if (p == NULL) {
		fprintf(stderr, "ERROR: malloc for soundfile_vio_buffer failed.\n");
		free(data);
		sf_close(sf);
		return NULL;
	}
	p->data = data;
	p->offset = 0;
	p->length = length;
	if (bytes) {
		/* Decoding compressed data into headerless PCM-encoded data. */
		SNDFILE *dest;
		dest = sf_open_virtual(&vio, SFM_WRITE, &info, p);
		if (dest == NULL) {
			fprintf(stderr, "ERROR: sf_open_virtual() failed (%s).\n", sf_strerror(NULL));
			free(data);
			free(p);
			sf_close(sf);
			return NULL;
		}
		lseek(fd, start, SEEK_SET);
		start = 0; /* No header. */
		if (bytes == 2) soundfile_copy_short(dest, sf, info.channels);
		else soundfile_copy_int(dest, sf, info.channels);
		p->offset = 0;
		sfinfo->format = info.format;
		sfinfo->channels = info.channels;
		sfinfo->samplerate = info.samplerate;
	} else {
		/* Copy header + sound data. */
		lseek(fd, 0, SEEK_SET);
		read(fd, data, length);
	}
	ret = sf_close(sf); /* fd closed, too. */
	if (ret != 0) {
		fprintf(stderr, "ERROR: sf_close() failed (%s).\n", sf_strerror(sf));
		free(data);
		free(p);
		return NULL;
	}
	p->start = start;
	sf = sf_open_virtual(&vio, SFM_READ, sfinfo, p);
	if (sf == NULL)
		fprintf(stderr, "ERROR: sf_open_virtual() failed (%s).\n", sf_strerror(NULL));
	*obj = p;
	return sf;
}

/* RING BUFFER */

void copy_from_ring_buffer(SAMPLE *dest, SAMPLE *ring_buffer,
                           unsigned long bufsize,
                           unsigned long write_offset, unsigned long items)
{
	if (bufsize <= items) {
		if (write_offset == 0) {
			memcpy(dest, ring_buffer, bufsize * sizeof(SAMPLE));
		} else {
			unsigned long last = bufsize - write_offset;
			memcpy(dest, ring_buffer + write_offset, last * sizeof(SAMPLE));
			memcpy(dest + last, ring_buffer, write_offset * sizeof(SAMPLE));
		}
	} else if (items < write_offset) {
		memcpy(dest, ring_buffer + write_offset - items, items * sizeof(SAMPLE));
	} else {
		if (write_offset == 0) {
			memcpy(dest, ring_buffer + bufsize - items, items * sizeof(SAMPLE));
		} else {
			unsigned long os2 = items - write_offset;
			unsigned long os1 = bufsize - os2;
			memcpy(dest, ring_buffer + os1, os2 * sizeof(SAMPLE));
			memcpy(dest + os2, ring_buffer, write_offset * sizeof(SAMPLE));
		}
	}
}

void copy_to_ring_output_buffer(SAMPLE *ring_buffer, SAMPLE *src,
                                unsigned long bufsize,
                                unsigned long read_offset, unsigned long items)
{
	unsigned long last = bufsize - read_offset;
	SAMPLE *ring_buffer_right = ring_buffer + read_offset;

	if (bufsize <= items) {
		while (last--)
			*ring_buffer_right++ += *src++;
		while (read_offset--)
			*ring_buffer++ += *src++;
	} else if (items <= last) {
		while (items--)
			*ring_buffer_right++ += *src++;
	} else if (read_offset == 0) {
		while (items--)
			*ring_buffer++ += *src++;
	} else {
		unsigned long os2 = bufsize - read_offset;
		unsigned long os1 = items - os2;
		while (os2--)
			*ring_buffer_right++ += *src++;
		while (os1--)
			*ring_buffer++ += *src++;
	}
}

/* COMPLEX TYPES */

struct sample_complex {
	SAMPLE real;
	SAMPLE imag;
};

struct sample_polar {
	SAMPLE mag;
	SAMPLE phase;
};

/* Destructive conversion from rect to polar */
void complex_to_polar(struct sample_complex *p, unsigned long size)
{
	while (size--) {
		SAMPLE mag = hypot(p->real, p->imag);
		p->imag = atan2(p->imag, p->real);
		p->real = mag;
		p++;
	}
}

/* Destructive conversion from polar to rect */
void polar_to_complex(struct sample_polar *p, unsigned long size)
{
	while (size--) {
		SAMPLE real = p->mag * cos(p->phase);
		p->phase = p->mag * sin(p->phase);
		p->mag = real;
		p++;
	}
}

/* ANALYSIS */

void apply_window(SAMPLE *buf, SAMPLE *win, unsigned long winsize)
{
	unsigned long i;

	for (i = 0; i < winsize; i++)
		buf[i] *= win[i];
}

void apply_scaled_window(SAMPLE *buf, SAMPLE *win, unsigned long winsize,
                         SAMPLE mult)
{
	unsigned long i;

	for (i = 0; i < winsize; i++)
		buf[i] *= win[i] * mult;
}

void apply_scaled_rectwin(SAMPLE *buf, unsigned long winsize, SAMPLE mult)
{
	unsigned long i;

	for (i = 0; i < winsize; i++)
		buf[i] *= mult;
}

void apply_zero_padding(SAMPLE *buf, unsigned long offset, unsigned long size)
{
	if (offset < size)
		memset(buf + offset, 0, (size - offset) * sizeof(SAMPLE));
}

void pconv_multiply_partitions(SAMPLE *out_beg, SAMPLE *fdl_beg, SAMPLE *ir,
                               unsigned long fdl_size,
                               unsigned long fdl_read_head,
                               unsigned long block_size,
                               unsigned long partitions)
{
	SAMPLE *fdl, *fdl_end, *out_end;

	fdl = fdl_beg + fdl_read_head;
	fdl_end = fdl_beg + fdl_size;
	out_end = out_beg + block_size;
	memset(out_beg, 0, sizeof(SAMPLE) * block_size);
	while (partitions--) {
		SAMPLE *out;
		for (out = out_beg; out < out_end; fdl += 2, ir += 2) {
			/* out_real += fdl_real * ir_real - fdl_imag * ir_imag */
			*out++ += fdl[0] * ir[0] - fdl[1] * ir[1];
			/* out_imag += fdl_real * ir_imag + fdl_imag * ir_real */
			*out++ += fdl[0] * ir[1] + fdl[1] * ir[0];
		}
		if (fdl >= fdl_end)
			fdl = fdl_beg;
	}
}

/* MIDI */

#define PM_TIMESTAMP_SIZE  4
#define PM_EVENT_SIZE      8

#define MIDI_BULK_TUNING_DUMP_NAME_INDEX         6
#define MIDI_BULK_TUNING_DUMP_NAME_LENGTH       16
#define MIDI_BULK_TUNING_DUMP_FREQ_DATA_INDEX   22

#define update_tuning_freq(ftype,freqs,ev,i,x,y,z)                                       \
	do {                                                                             \
		if ((ev[x] | ev[y] | ev[z]) & 0x80)                                      \
			return i;                                                        \
		if ((ev[x] & ev[y] & ev[z]) != 0x7f)                                     \
			freqs[i] = (ftype)                                               \
				(8.1758 * pow(2, (ev[x] + ((ftype) (ev[y] << 7 | ev[z])  \
				              / (1 << 14))) / 12));                      \
	} while (0)

/*
 * Set an array of frequencies with the data received from a MIDI bulk
 * tuninig dump message and update the name of the scale.
 *
 * ev is a pointer to an array of PortMidi events (PmEvent structure).
 */
int set_freqs_from_midi(unsigned char *ev, SAMPLE *freqs, char *name)
{
	int x, y, i, j;

	/* Tuning 16 ASCII characters name. */
	i = MIDI_BULK_TUNING_DUMP_NAME_INDEX + PM_TIMESTAMP_SIZE;
	for (j = 0; j < MIDI_BULK_TUNING_DUMP_NAME_LENGTH; i++, j++) {
		if (j % 4 == MIDI_BULK_TUNING_DUMP_NAME_INDEX % 4)
			i += PM_TIMESTAMP_SIZE;
		name[j] = ev[i];
	}
	name[j] = 0;

	/*
	 * A loop cycle crosses three PortMidi events (24 bytes).
	 *
	 *           |loopstart      loopend|
	 *     xy____zxyz____xyzx____yzxy____
	 *
	 *
	 *     x, y, z: frequency data for one note
	 *
	 *     ____: timestamp to skip (4 bytes)
	 *
	 */
	x = MIDI_BULK_TUNING_DUMP_FREQ_DATA_INDEX * 2
	    - (MIDI_BULK_TUNING_DUMP_FREQ_DATA_INDEX % PM_TIMESTAMP_SIZE);

	i = x + 2 + PM_TIMESTAMP_SIZE;
	for (j = 0; j < 128; i += PM_EVENT_SIZE, j++) {
		y = x + 1;
		update_tuning_freq(SAMPLE, freqs, ev, j++, x, y, i);
		update_tuning_freq(SAMPLE, freqs, ev, j++, i + 1, i + 2, i + 3);
		i += PM_EVENT_SIZE;
		update_tuning_freq(SAMPLE, freqs, ev, j++, i, i + 1, i + 2);
		x = i + 3;
		i += PM_EVENT_SIZE;
		update_tuning_freq(SAMPLE, freqs, ev, j, x, i, i + 1);
		x = i + 2;
	}
	return 0;
}

int set_ffreqs_from_midi(unsigned char *ev, float *freqs)
{
	int x, y, i, j;

	x = MIDI_BULK_TUNING_DUMP_FREQ_DATA_INDEX * 2
	    - (MIDI_BULK_TUNING_DUMP_FREQ_DATA_INDEX % PM_TIMESTAMP_SIZE);

	i = x + 2 + PM_TIMESTAMP_SIZE;
	for (j = 0; j < 128; i += PM_EVENT_SIZE, j++) {
		y = x + 1;
		update_tuning_freq(float, freqs, ev, j++, x, y, i);
		update_tuning_freq(float, freqs, ev, j++, i + 1, i + 2, i + 3);
		i += PM_EVENT_SIZE;
		update_tuning_freq(float, freqs, ev, j++, i, i + 1, i + 2);
		x = i + 3;
		i += PM_EVENT_SIZE;
		update_tuning_freq(float, freqs, ev, j, x, i, i + 1);
		x = i + 2;
	}
	return 0;
}

/*
 * freqs is the array of the frequencies to update from the frequency
 * data obtained via MIDI SysEx (no PortMidi).
 */
int set_freqs_from_midi_data_format(SAMPLE *freqs, char *midi_freq_data,
                                    unsigned int size)
{
	int i;
	char *d;

	d = midi_freq_data;
	for (i = 0; i < size; i++) {
		SAMPLE s;
		char x, y, z;
		x = *d++;
		y = *d++;
		z = *d;
		if ((x | y | z) & 0x80)
			return i;  /* Not 21 bits data word. */
		if ((x & y & z) == 0x7f)
			continue;  /* 0x7f 0x7f 0x7f means "no change". */
		d++;
		s = (SAMPLE) (x + ((SAMPLE) (y << 7 | z)) / (1 << 14));
		freqs[i] = (SAMPLE) (8.1758 * pow(2, s / 12));
	}
	return 0;
}

int set_ffreqs_from_midi_data_format(float *freqs, char *midi_freq_data,
                                     unsigned int size)
{
	int i;
	char *d;

	d = midi_freq_data;
	for (i = 0; i < size; i++) {
		float s;
		char x, y, z;
		x = *d++;
		y = *d++;
		z = *d;
		if ((x | y | z) & 0x80)
			return i;  /* Not 21 bits data word. */
		if ((x & y & z) == 0x7f)
			continue;  /* 0x7f 0x7f 0x7f means "no change". */
		d++;
		s = (float) (x + ((float) (y << 7 | z)) / (1 << 14));
		freqs[i] = (float) (8.1758 * pow(2, s / 12));
	}
	return 0;
}

/* MISC */

/* Circular shift to right. */
void circular_rshift(void *buf, void *tmp, size_t bytes, size_t size)
{
	char *rptr = (char*) buf + size;
	char *lptr = rptr - bytes;
	int i;

	memcpy(tmp, lptr, bytes);
	for (i = size; i > bytes; i--)
		*--rptr = *--lptr;
	memcpy(buf, tmp, bytes);
}

/* Circular shift to left. */
void circular_lshift(void *buf, void *tmp, size_t bytes, size_t size)
{
	char *lptr = (char*) buf;
	char *rptr = lptr + bytes;
	int i;

	memcpy(tmp, buf, bytes);
	for (i = 0; i < size - bytes; i++)
		*lptr++ = *rptr++;
	memcpy(lptr, tmp, bytes);
}

#ifndef WIN32
static struct sigaction oldact = { .sa_handler = SIG_IGN };

int ignore_sigtstp()
{
	struct sigaction act;

	sigaction(SIGTSTP, NULL, &act);
	if (act.sa_handler != SIG_IGN) {
		act.sa_handler = SIG_IGN;
		if (sigaction(SIGTSTP, &act, &oldact) != 0)
			return -1;
	}
	return 0;
}

int reset_sigtstp_handler()
{
	struct sigaction act;

	sigaction(SIGTSTP, NULL, &act);
	if (act.sa_handler == SIG_IGN &&
	    oldact.sa_handler != SIG_IGN &&
	    sigaction(SIGTSTP, &oldact, NULL) != 0)
		return -1;
	return 0;
}
#endif
