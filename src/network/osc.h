/*
 * Copyright (c) 2015-2023 Tito Latini
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

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/select.h>
#endif

#define MEM_FREE_INDEX  1

#define DATA_INDEX_OFFSET  2

#define REQUIRED_VALUES_INDEX  0

#define SLIP_ENCODING_FLAG  1
#define COUNT_PREFIX_FLAG   2

#define OSC_MESSAGE_LENGTH_SIZE  4

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

#ifndef WIN32
typedef int OSC_SOCKET;
typedef int SOCKOPT_VALUE;
typedef unsigned char BUFFER_DATATYPE;

#define OSC_INVALID_SOCKET -1

#define SERVER_SOCKET(x)  (x)->servfd
#define SERVER_FD(x)      (x)->servfd
#define GET_SOCKET(x,i)   i
#define CLOSE_SOCKET(x)   close(x)

#define IS_NONBLOCKING(obj,addr)  osc_getsock_nonblock((obj)->servfd)

/* A valid fd is monitored by select(). */
#define AVAILABLE_FD(x) 1

#define FD_LOOP(i, nfds, fds, obj)                    \
	for (i = 0; i < nfds; i++)                    \
		if (FD_ISSET(i, &(fds)))
#else
#define WSA_REQUIRED_VERSION  MAKEWORD(2,2)

typedef SOCKET OSC_SOCKET;
typedef char SOCKOPT_VALUE;
typedef char BUFFER_DATATYPE;

#define OSC_INVALID_SOCKET  INVALID_SOCKET

#define SERVER_SOCKET(x)  (x)->fd_array[0]
#define SERVER_FD(x)      0
#define GET_SOCKET(x,i)   (x)->fd_array[i]
#define CLOSE_SOCKET(x)   closesocket(x)

#define IS_NONBLOCKING(obj,addr)  (addr)->non_blocking

#define AVAILABLE_FD(x)  ((x)->newfd < FD_SETSIZE)

#define FD_LOOP(i, nfds, fds, obj)                    \
	for (i = 0; i < nfds; i++)                    \
		if (FD_ISSET(GET_SOCKET(obj,i), &(fds)))
#endif

struct osc_address {
	struct addrinfo *info;
	struct sockaddr_storage *saddr;
	socklen_t saddr_len;
#ifdef WIN32
	int non_blocking;
#endif
};

struct osc_fds {
	int maxfd;      /* highest-numbered fd */
#ifndef WIN32
	int servfd;     /* server fd */
#endif
	int lastfd;     /* fd used for the last received message */
	int count;      /* number of the connections */
#ifdef WIN32
	int newfd;      /* fd_array index for a new socket */
	SOCKET fd_array[FD_SETSIZE];
#endif
	fd_set fds;
};

#define SINGLE_MESSAGE       1
#define BUNDLE_LAST_MESSAGE  2
#define BLOB_VALUE           4

#define IS_SINGLE_MESSAGE(x)  (x & SINGLE_MESSAGE)
#define IS_LAST_MESSAGE(x)    (x & (SINGLE_MESSAGE | BUNDLE_LAST_MESSAGE))
#define IS_BLOB(x)            (x & BLOB_VALUE)

#define OSC_FIX_SIZE(n)  ((n) + 4 - ((n) & 3))

#ifdef WIN32
int initialize_winsock(void);
#endif
static void osc_move_data_left(uint32_t *start, uint32_t *end, unsigned int n);
static void osc_move_data_right(uint32_t *start, uint32_t *end, unsigned int n);
static int is_slip_msg(const unsigned char *buf, int len);

int osc_address_new(struct osc_address **addr, const char *host,
                    unsigned int port, int is_datagram, int is_input,
                    int hints_flags);
void osc_address_free(struct osc_address *a);
int check_osc_pattern(void *buf, const char *addrpat, const char *types);
int index_osc_values(void *buf, void *ibuf, char *tbuf,
                     unsigned int types_start, unsigned int data_start);
#ifdef LITTLE_ENDIAN
int index_osc_values_le(void *buf, void *ibuf, char *tbuf,
                        unsigned int types_start, unsigned int data_start);
#endif
int index_osc_bundle_values(void *buf, void *ibuf, char *tbuf,
                            unsigned int size, int swap);
unsigned int osc_start_message(void *buf, unsigned int maxlen, void *ibuf,
                               char *tbuf, const char *addrpat,
                               const char *types);
unsigned int osc_bundle_append_message(void *buf, void *ibuf, char *tbuf,
                                       const char *address, const char *types,
                                       unsigned int offset);
int osc_maybe_reserve_space(void *oscbuf, void *ibuf, unsigned int index,
                            unsigned int value, int flags);
static int osc_fix_length(uint32_t *start, uint32_t *value, uint32_t *end, int diff);
struct osc_fds *osc_alloc_fds(void);
void osc_set_servfd(struct osc_fds *o, OSC_SOCKET servfd);
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
int osc_getsock_broadcast(OSC_SOCKET sockfd);
int osc_setsock_broadcast(OSC_SOCKET sockfd, const struct addrinfo *info, int is_set);
int osc_getsock_nonblock(OSC_SOCKET sockfd);
int osc_setsock_nonblock(OSC_SOCKET sockfd, int is_nonblock);
int osc_setsock_reuseaddr(OSC_SOCKET sockfd);
unsigned int sizeof_socklen(void);

#endif  /* __INCUDINE_OSC_H */
