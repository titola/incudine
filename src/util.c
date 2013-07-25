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

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>
#include <sndfile.h>
#include <math.h>
#include "common.h"

#ifdef __INCUDINE_USE_64_BIT_SAMPLE__
#define sf_readf_SAMPLE  sf_readf_double
#else
#define sf_readf_SAMPLE  sf_readf_float
#endif

/*
 * Thomas Wang's integer hash.
 * http://www.concentric.net/~Ttwang/tech/inthash.htm
 */
int32_t int_hash(int32_t key)
{
    uint32_t x = (uint32_t) key;
    x  = ~x + (x << 15);  /* x = (x << 15) - x - 1; */
    x ^=  x >> 10;
    x +=  x << 2;
    x ^=  x >> 4;
    x *=  2057;           /* x = (x + (x << 3)) + (x << 11); */
    x ^=  x >> 16;
    return (int32_t) x;
}

/* THREAD */

int pthread_set_priority(pthread_t thread, int priority)
{
    int policy, minprio;
    struct sched_param param;

    pthread_getschedparam (thread, &policy, &param);
#ifdef LINUX
    policy = SCHED_FIFO;
#else
    policy = SCHED_RR;
#endif
    minprio = sched_get_priority_min(policy);
    if (priority < minprio) {
        priority = minprio;
    } else {
        int maxprio = sched_get_priority_max(policy);
        if (priority > maxprio)
            priority = maxprio;
    }
    param.sched_priority = priority;
    return pthread_setschedparam(thread, policy, &param);
}

/* MEMORY */

SAMPLE *foreign_alloc_sample(size_t size)
{
    size_t n = size*sizeof(SAMPLE);
    SAMPLE *ptr = (SAMPLE*) malloc(n);

    if (ptr != NULL)
        memset(ptr, 0, n);
    return ptr;
}

void *foreign_zero_sample(SAMPLE *ptr, size_t n)
{
    return memset(ptr, 0, sizeof(SAMPLE)*n);
}

/* TEMPO */

SAMPLE tempo_sync(SAMPLE *now, SAMPLE period)
{
    return *now + period - fmod(*now, period);
}

/* SNDFILE */

#define min(a,b) ((a) < (b) ? (a) : (b))

void sndfile_to_buffer(SAMPLE *buf, SNDFILE *sndfile, unsigned long frames,
                       int channels, unsigned long buf_offset, int chunk_frames)
{
    sf_count_t n, incr, remain = frames;
    SAMPLE *p;

    if (remain < chunk_frames)
        chunk_frames = remain;
    incr = chunk_frames * channels;
    for (p = buf+buf_offset*channels; remain > 0; p += incr) {
        n = sf_readf_SAMPLE(sndfile, p, chunk_frames);
        if (n == 0) {
            fprintf(stderr, "sndfile_to_buffer fails (%s)\n",
                    sf_strerror(sndfile));
            break;
        }
        remain -= n;
        if (remain < chunk_frames) {
            chunk_frames = remain;
            incr = remain * channels;
        }
    }
}

void map_sndfile_ch_to_buffer(SAMPLE *buf, SNDFILE *sndfile, unsigned long frames,
                              int channels, int buf_channels, unsigned long buf_offset,
                              int chunk_frames, int* channel_map_dest, int* channel_map_src,
                              int channel_map_size)
{
    sf_count_t i, n, k, incr, remain = frames;
    SAMPLE *p, *q, *curr;

    if (remain < chunk_frames)
        chunk_frames = remain;
    incr = chunk_frames * channels;
    p = buf+buf_offset*buf_channels;
    q = (SAMPLE*) malloc(incr*sizeof(SAMPLE));
    while (remain > 0) {
        n = sf_readf_SAMPLE(sndfile, q, chunk_frames);
        if (n == 0) {
            fprintf(stderr, "sndfile_ch_to_buffer fails (%s)\n",
                    sf_strerror(sndfile));
            break;
        }
        remain -= n;
        for (i=0, curr=q; i<incr; i += channels, curr += channels, p += buf_channels) {
            for (k=0; k<channel_map_size; k++)
                p[channel_map_dest[k]] = curr[channel_map_src[k]];
        }
        if (remain < chunk_frames) {
            chunk_frames = remain;
            incr = remain * channels;
        }
    }
    free(q);
}

/* RING BUFFER */

void copy_from_ring_buffer(SAMPLE *dest, SAMPLE *ring_buffer,
                           unsigned long buffer_size, unsigned long write_offset,
                           unsigned long items)
{
    if (buffer_size <= items) {
        if (write_offset == 0) {
            memcpy(dest, ring_buffer, buffer_size*sizeof(SAMPLE));
        } else {
            unsigned long last = buffer_size - write_offset;

            memcpy(dest, ring_buffer + write_offset, last*sizeof(SAMPLE));
            memcpy(dest + last, ring_buffer, write_offset*sizeof(SAMPLE));
        }
    } else if (items <= write_offset) {
        memcpy(dest, ring_buffer + (write_offset - items), items*sizeof(SAMPLE));
    } else {
        if (write_offset == 0) {
            memcpy(dest, ring_buffer + (buffer_size - items), items*sizeof(SAMPLE));
        } else {
            unsigned long offset2 = items - write_offset;
            unsigned long offset1 = buffer_size - offset2;

            memcpy(dest, ring_buffer + offset1, offset2*sizeof(SAMPLE));
            memcpy(dest + offset2, ring_buffer, write_offset);
        }
    }
}

void copy_to_ring_output_buffer(SAMPLE *ring_buffer, SAMPLE *src,
                                unsigned long buffer_size, unsigned long read_offset,
                                unsigned long items)
{
    int i;
    unsigned long last = buffer_size - read_offset;
    SAMPLE *ring_buffer_right = ring_buffer + read_offset;

    if (buffer_size <= items) {
        for (i=0; i<last; i++)
            *ring_buffer_right++ += *src++;

        for (i=0; i<read_offset; i++)
            *ring_buffer++ += *src++;
    } else if (items <= last) {
        for (i=0; i<items; i++)
            *ring_buffer_right++ += *src++;
    } else if (read_offset == 0) {
        for (i=0; i<items; i++)
            *ring_buffer++ += *src++;
    } else {
        unsigned long offset2 = buffer_size - read_offset;
        unsigned long offset1 = items - offset2;

        for (i=0; i<offset2; i++)
            *ring_buffer_right++ += *src++;

        for (i=0; i<offset1; i++)
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
    unsigned long i;
    SAMPLE mag;

    for (i=0; i<size; i++, p++) {
        mag     = hypot(p->real, p->imag);
        p->imag = atan2(p->imag, p->real);
        p->real = mag;
    }
}

/* Destructive conversion from polar to rect */
void polar_to_complex(struct sample_polar *p, unsigned long size)
{
    unsigned long i;
    SAMPLE real;

    for (i=0; i<size; i++, p++) {
        real     = p->mag * cos(p->phase);
        p->phase = p->mag * sin(p->phase);
        p->mag   = real;
    }
}

/* ANALYSIS */

void apply_window(SAMPLE *buffer, SAMPLE *window, unsigned long winsize)
{
    unsigned long i;

    for (i=0; i<winsize; i++)
        buffer[i] *= window[i];
}

void apply_scaled_window(SAMPLE *buffer, SAMPLE *window,
                         unsigned long winsize, unsigned long size)
{
    unsigned long i;
    SAMPLE mult = (SAMPLE) 1.0 / size;

    for (i=0; i<winsize; i++)
        buffer[i] *= window[i] * mult;
}

void apply_scaled_rectwin(SAMPLE *buffer, unsigned long winsize,
                          unsigned long size)
{
    unsigned long i;
    SAMPLE mult = (SAMPLE) 1.0 / size;

    for (i=0; i<winsize; i++)
        buffer[i] *= mult;
}

void apply_zero_padding(SAMPLE *buffer, unsigned long offset,
                        unsigned long size)
{
    if (offset < size)
        memset(buffer+offset, 0, (size-offset)*sizeof(SAMPLE));
}
