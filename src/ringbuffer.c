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

#include "ringbuffer.h"

static unsigned int __next_pow_of_two(unsigned int n)
{
	unsigned int i;

	for(i = 1; i < n; i *= 2);
	return i;
}

struct incudine_ringbuffer *incudine_ringbuffer_create(unsigned int size)
{
	struct incudine_ringbuffer *rb;

	rb = (struct incudine_ringbuffer *)
		malloc(sizeof(struct incudine_ringbuffer));
	if (rb == NULL)
		return NULL;
	size = __next_pow_of_two(size);
	rb->buf = (char *) malloc(size);
	if (rb->buf == NULL) {
		free(rb);
		return NULL;
	}
	rb->read_head = 0;
	rb->write_head = 0;
	rb->size = size;
	rb->mask = size - 1;
	return rb;
}

void incudine_ringbuffer_free(struct incudine_ringbuffer *rb)
{
	if (rb != NULL) {
		if (rb->buf != NULL) {
			free(rb->buf);
			rb->buf = NULL;
		}
		free(rb);
	}
}

void incudine_ringbuffer_reset(struct incudine_ringbuffer *rb)
{
	write_memory_barrier();
	rb->write_head = 0;
	rb->read_head = 0;
}

int incudine_ringbuffer_empty(struct incudine_ringbuffer *rb)
{
	unsigned int read_head, write_head;

	read_head = READ_ONCE(rb->read_head);
	write_head = READ_ONCE(rb->write_head);
	read_memory_barrier();
	return read_head == write_head;
}

unsigned int incudine_ringbuffer_write(struct incudine_ringbuffer *rb,
                                       char *src, unsigned int bytes)
{
	unsigned int n, write_end;

	n = incudine_ringbuffer_write_space(rb);
	if (bytes > n)
		bytes = n;
	write_end = (rb->write_head + bytes - rb->size) & rb->mask;
	if (rb->write_head > write_end) {
		n = rb->size - rb->write_head;
		memcpy(rb->buf + rb->write_head, src, n);
		memcpy(rb->buf, src + n, write_end);
	} else {
		memcpy(rb->buf + rb->write_head, src, bytes);
	}
	write_memory_barrier();
	WRITE_ONCE(rb->write_head, write_end);
	return bytes;
}

unsigned int incudine_ringbuffer_write_space(struct incudine_ringbuffer *rb)
{
	unsigned int n, read_head;

	read_head = READ_ONCE(rb->read_head);
	read_memory_barrier();
	n = (read_head - rb->write_head - 1) & rb->mask;
	return n;
}

void incudine_ringbuffer_write_advance(struct incudine_ringbuffer *rb,
                                       unsigned int bytes)
{
	write_memory_barrier();
	WRITE_ONCE(rb->write_head, (rb->write_head + bytes - rb->size) & rb->mask);
}

unsigned int incudine_ringbuffer_read(struct incudine_ringbuffer *rb,
                                      char *dest, unsigned int bytes)
{
	unsigned int n, read_end;

	n = incudine_ringbuffer_read_space(rb);
	if (bytes > n)
		bytes = n;
	read_end = (rb->read_head + bytes - rb->size) & rb->mask;
	if (rb->read_head > read_end) {
		n = rb->size - rb->read_head;
		memcpy(dest, rb->buf + rb->read_head, n);
		memcpy(dest + n, rb->buf, read_end);
	} else {
		memcpy(dest, rb->buf + rb->read_head, bytes);
	}
	write_memory_barrier();
	WRITE_ONCE(rb->read_head, read_end);
	return bytes;
}

unsigned int incudine_ringbuffer_peek(struct incudine_ringbuffer *rb,
                                      char *dest, unsigned int bytes)
{
	unsigned int n, read_end;

	n = incudine_ringbuffer_read_space(rb);
	if (bytes > n)
		bytes = n;
	read_end = (rb->read_head + bytes - rb->size) & rb->mask;
	if (rb->read_head > read_end) {
		n = rb->size - rb->read_head;
		memcpy(dest, rb->buf + rb->read_head, n);
		memcpy(dest + n, rb->buf, read_end);
	} else {
		memcpy(dest, rb->buf + rb->read_head, bytes);
	}
	return bytes;
}

unsigned int incudine_ringbuffer_read_space(struct incudine_ringbuffer *rb)
{
	unsigned int n, write_head;

	write_head = READ_ONCE(rb->write_head);
	read_memory_barrier();
	n = (write_head - rb->read_head) & rb->mask;
	return n;
}

void incudine_ringbuffer_read_advance(struct incudine_ringbuffer *rb,
                                      unsigned int bytes)
{
	write_memory_barrier();
	WRITE_ONCE(rb->read_head, (rb->read_head + bytes - rb->size) & rb->mask);
}
