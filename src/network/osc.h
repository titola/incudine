/*
 * Copyright (c) 2015-2016 Tito Latini
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

#ifndef __INCUDINE_OSC_H
#define __INCUDINE_OSC_H

#include <arpa/inet.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#define MEM_FREE_INDEX  1

#define DATA_INDEX_OFFSET  2

#define REQUIRED_VALUES_INDEX  0

#define SLIP_ENCODING_FLAG  1
#define COUNT_PREFIX_FLAG   2

#define OSC_MSGTOOLONG  -2
#define OSC_BADMSG      -3

/*
 * SLIP (RFC 1055)
 * Transmission of IP datagrams over serial lines.
 */
#define SLIP_END      0300
#define SLIP_ESC      0333
#define SLIP_ESC_END  0334
#define SLIP_ESC_ESC  0335

struct osc_address {
        struct addrinfo *info;
        struct sockaddr_storage *saddr;
        socklen_t saddr_len;
};

struct osc_fds {
        fd_set fds;
        int maxfd;      /* highest-numbered fd */
        int servfd;     /* server fd */
        int lastfd;     /* fd used for the last received message */
        int count;      /* number of the connections */
};

#define osc_fix_size(n)  (n + 4 - (n & 3))

#define FD_LOOP(i, nfds, fds)                         \
        for (i = 0; i < nfds; i++)                    \
                if (FD_ISSET(i, &(fds)))

static void osc_move_data_left(uint32_t *start, uint32_t *end, unsigned int n);
static void osc_move_data_right(uint32_t *start, uint32_t *end, unsigned int n);
static int is_slip_msg(const unsigned char *buf, int len);

int osc_address_new(struct osc_address **addr, const char *host,
                    unsigned int port, int is_datagram, int is_input);
void osc_address_free(struct osc_address *a);
int check_osc_pattern(void *buf, const char *addrpat, const char *types);
int index_osc_values(void *buf, void *ibuf, char *tbuf,
                     unsigned int types_start, unsigned int data_start);
#ifdef LITTLE_ENDIAN
int index_osc_values_le(void *buf, void *ibuf, char *tbuf,
                        unsigned int types_start, unsigned int data_start);
#endif
unsigned int osc_start_message(void *buf, unsigned int bufsize, void *ibuf,
                               char *tbuf, const char *addrpat,
                               const char *types);
int osc_maybe_reserve_space(void *oscbuf, void *ibuf, unsigned int index,
                            unsigned int data_size);
struct osc_fds *osc_alloc_fds(void);
void osc_set_servfd(struct osc_fds *o, int servfd);
int osc_lastfd(struct osc_fds *o);
int osc_connections(struct osc_fds *o);
int osc_next_fd_set(struct osc_fds *o, int curr);
void osc_close_connections(struct osc_fds *o);
int osc_close_server(struct osc_fds *o);
int osc_recv(struct osc_fds *o, struct osc_address *addr, void *buf,
             unsigned int maxlen, int is_slip, int flags);
unsigned int osc_slip_encode(const unsigned char *src, unsigned char *dest,
                             unsigned int len);
unsigned int osc_slip_decode(unsigned char *buf, unsigned int maxlen);
int osc_getsock_broadcast(int sockfd);
int osc_setsock_broadcast(int sockfd, const struct addrinfo *info, int is_set);
int osc_getsock_nonblock(int sockfd);
int osc_setsock_nonblock(int sockfd, int is_nonblock);
int osc_setsock_reuseaddr(int sockfd);
unsigned int osc_strsize(const char *s);
unsigned int sizeof_socklen(void);

#endif  /* __INCUDINE_OSC_H */
