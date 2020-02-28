/*
 * Copyright (c) 2020 Tito Latini
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

#ifndef __INCUDINE_RINGBUFFER_H
#define __INCUDINE_RINGBUFFER_H

#include <stdlib.h>
#include <string.h>
#include "barrier.h"

struct incudine_ringbuffer {
        char *buf;
        unsigned int write_head;
        unsigned int read_head;
        unsigned int size;
        unsigned int mask;
};

/* Note: sizeof(var) doesn't exceed the word size of the machine. */
#define WRITE_ONCE(var, value) \
        (*((volatile typeof(value) *)(&(var))) = (value))

#define READ_ONCE(var) (*((volatile typeof(var) *)(&(var))))

struct incudine_ringbuffer *incudine_ringbuffer_create(unsigned int size);
void incudine_ringbuffer_free(struct incudine_ringbuffer *rb);
void incudine_ringbuffer_reset(struct incudine_ringbuffer *rb);
int incudine_ringbuffer_empty(struct incudine_ringbuffer *rb);
unsigned int incudine_ringbuffer_write(struct incudine_ringbuffer *rb,
                                       char *src, unsigned int bytes);
unsigned int incudine_ringbuffer_write_space(struct incudine_ringbuffer *rb);
void incudine_ringbuffer_write_advance(struct incudine_ringbuffer *rb,
                                       unsigned int bytes);
unsigned int incudine_ringbuffer_read(struct incudine_ringbuffer *rb,
                                      char *dest, unsigned int bytes);
unsigned int incudine_ringbuffer_peek(struct incudine_ringbuffer *rb,
                                      char *dest, unsigned int bytes);
unsigned int incudine_ringbuffer_read_space(struct incudine_ringbuffer *rb);
void incudine_ringbuffer_read_advance(struct incudine_ringbuffer *rb,
                                      unsigned int bytes);

#endif  /* __INCUDINE_RINGBUFFER_H */
