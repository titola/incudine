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

#include "osc.h"

int osc_address_new(struct osc_address **addr, const char *host,
                    unsigned int port, int is_datagram, int is_input,
                    int hints_flags)
{
	struct addrinfo hints, *info;
	struct osc_address *a;
	char service[6];
	int ret;

	memset(&hints, 0, sizeof hints);
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = is_datagram ? SOCK_DGRAM : SOCK_STREAM;
	hints.ai_flags = hints_flags;

	snprintf(service, 6, "%d", port);
	if ((ret = getaddrinfo(host, service, &hints, &info)) != 0)
		return ret;

	a = (struct osc_address *) malloc(sizeof(struct osc_address));
	if (a == NULL) {
		freeaddrinfo(info);
		return -1;
	}
	if (is_input) {
		a->saddr = (struct sockaddr_storage *)
			malloc(sizeof(struct sockaddr_storage));
		if (a->saddr == NULL) {
			freeaddrinfo(info);
			free(a);
			return -1;
		}
		a->saddr_len = sizeof(struct sockaddr_storage);
	} else {
		a->saddr = (struct sockaddr_storage *) info->ai_addr;
		a->saddr_len = info->ai_addrlen;
	}
	a->info = info;
#ifdef WIN32
	a->non_blocking = 0;
#endif
	*addr = a;
	return 0;
}

void osc_address_free(struct osc_address *a)
{
	if (a != NULL) {
		if (a->saddr != NULL) {
			if (a->saddr != (struct sockaddr_storage *) a->info->ai_addr)
				free(a->saddr);
			a->saddr = NULL;
		}
		if (a->info != NULL) {
			freeaddrinfo(a->info);
			a->info = NULL;
		}
		free(a);
	}
}

/*
 * Return 1 if the OSC address pattern and the OSC type tag strings match.
 * Return 0 otherwise.
 */
int check_osc_pattern(void *buf, const char *address, const char *types)
{
	char *s1, *s2;
	int i;

	s1 = (char *) buf;
	s2 = (char *) address;

	for (i = 0; *s1 != 0 && *s2 != 0; i++)
		if (*s1++ != *s2++)
			return 0;

	if ((*s1 != *s2)
	    || (strcmp(types, (char *) (buf + OSC_FIX_SIZE(i) + 1)) != 0))
		return 0;

	return 1;
}

/*
 * Fill the array of pointers ibuf with the pointers to the memory
 * used to store the data of the received OSC message.
 */
int index_osc_values(void *oscbuf, void *ibuf, char *typebuf,
                     unsigned int types_start, unsigned int data_start)
{
	char *ttag, **res, **values, *tbuf;
	uint32_t *data;
	int required_values, i;

	res = (char **) ibuf;
	ttag = *res = ((char *) oscbuf) + types_start;
	data = (uint32_t *) (((char *) oscbuf) + data_start);
	required_values = typebuf[REQUIRED_VALUES_INDEX];
	/* The second slot is reserved for the pointer to the free memory. */
	values = res + DATA_INDEX_OFFSET + required_values;
	res = values;
	/*
	 * The first slot is reserved for the number of the required values.
	 * This value is greater than zero after bundle_append_message().
	 */
	tbuf = typebuf + 1 + required_values;

	i = 0;
	while (*ttag) {
		*res = (char *) &data[i];
		*tbuf = *ttag;
		switch (*ttag++) {
		case 'f':    /* float */
		case 'i':    /* int32 */
		case 'c':    /* char */
			i++;
			tbuf++;
			break;
		case 'd':    /* double float */
		case 'h':    /* int64 */
			i += 2;
			tbuf++;
			break;
		case 's':    /* string */
		case 'S':    /* symbol */
			i += strlen((char *) &data[i]) / 4 + 1;
			tbuf++;
			break;
		case 'b':    /* blob */
			i += OSC_FIX_SIZE(data[i] + 3) >> 2;
			tbuf++;
			break;
		case 't':    /* timetag */
			i += 2;
			tbuf++;
			break;
		case 'm':    /* midi */
			i++;
			tbuf++;
			break;
		default:
			/*
			 * Other required types in OSC 1.1:
			 *
			 *     T - TRUE
			 *     F - FALSE
			 *     N - NIL
			 *     I - INFINITUM
			 */
			continue;
		}
		res++;
	}
	/* Start of free memory. */
	*res = (char *) &data[i];
	/* Number of the required values. */
	typebuf[REQUIRED_VALUES_INDEX] = (char**) res - (char **) values;
	res = (char **) ibuf;
	res[MEM_FREE_INDEX] = (char *) &data[i];

	return i;
}

#ifdef LITTLE_ENDIAN
/*
 * Similar to `index_osc_values', but the order of the bytes are swapped
 * on little endian machines.
 */
int index_osc_values_le(void *oscbuf, void *ibuf, char *typebuf,
                        unsigned int types_start, unsigned int data_start)
{
	char *ttag, **res, **values, *tbuf;
	uint32_t *data, tmp;
	int i = 0;

	res = (char **) ibuf;
	ttag = *res = ((char *) oscbuf) + types_start;
	data = (uint32_t *) (((char *) oscbuf) + data_start);
	/* The second slot is reserved for the pointer to the free memory. */
	values = res + DATA_INDEX_OFFSET;
	res = values;
	/*
	 * The first slot is reserved for the number of the required values.
	 * Note: index_osc_values_le() is not called from
	 * osc_bundle_append_message(), therefore this is the first message.
	 */
	tbuf = typebuf + 1;

	while (*ttag) {
		*res = (char *) &data[i];
		*tbuf = *ttag;
		switch (*ttag++) {
		case 'f':    /* float */
		case 'i':    /* int32 */
		case 'c':    /* char */
			data[i] = htonl(data[i]);
			i++;
			tbuf++;
			break;
		case 'd':    /* double float */
		case 'h':    /* int64 */
			tmp = (uint32_t) data[i];
			data[i] = htonl(data[i+1]);
			data[i+1] = htonl(tmp);
			i += 2;
			tbuf++;
			break;
		case 's':    /* string */
		case 'S':    /* symbol */
			i += strlen((char *) &data[i]) / 4 + 1;
			tbuf++;
			break;
		case 'b':    /* blob */
			data[i] = htonl(data[i]);
			i += OSC_FIX_SIZE(data[i] + 3) >> 2;
			tbuf++;
			break;
		case 't':    /* timetag */
			/*
			 * No time tag semantics in OSC 1.1. However, the least
			 * significant bit is reserved to mean "immediately".
			 */
			i += 2;
			tbuf++;
			break;
		case 'm':    /* midi */
			i++;
			tbuf++;
			break;
		default:
			/*
			 * Other required types in OSC 1.1:
			 *
			 *     T - TRUE
			 *     F - FALSE
			 *     N - NIL
			 *     I - INFINITUM
			 */
			continue;
		}
		res++;
	}
	/* Start of free memory. */
	*res = (char *) &data[i];
	/* Number of the required values. */
	typebuf[REQUIRED_VALUES_INDEX] = (char**) res - (char **) values;
	res = (char **) ibuf;
	res[MEM_FREE_INDEX] = (char *) &data[i];
	*tbuf = 0;
	return i;
}
#endif  /* LITTLE_ENDIAN */

int index_osc_bundle_values(void *buf, void *ibuf, char *tbuf,
                            unsigned int size, int swap)
{
	char *msg, *len, *end;
	unsigned int required_values, ttag_start, slen, i;
#ifndef LITTLE_ENDIAN
	(void) swap;
#endif
	end = buf + size + OSC_MESSAGE_LENGTH_SIZE;
	/* Bounds checking.*/
	len = buf;
	while (len < end)
		len += htonl(*((uint32_t *) len)) + OSC_MESSAGE_LENGTH_SIZE;
	if (len != end) {
		fprintf(stderr, "ERROR: bounds checking failed in"
		                "index_osc_bundle_values()\n");
		return -1;
	}
	required_values = 0;
	len = buf;
	do {
		msg = len + OSC_MESSAGE_LENGTH_SIZE;
		slen = strlen(msg);
		i = OSC_FIX_SIZE(slen);
		ttag_start = i;
		slen = strlen(msg + i);
		i += OSC_FIX_SIZE(slen);
		tbuf[REQUIRED_VALUES_INDEX] = required_values;
#ifdef LITTLE_ENDIAN
		if (swap)
			index_osc_values_le(msg, ibuf, tbuf, ttag_start, i);
		else
#endif
			index_osc_values(msg, ibuf, tbuf, ttag_start, i);
		required_values += tbuf[REQUIRED_VALUES_INDEX];
		len = msg + htonl(*((uint32_t *) len));
	} while (len < end);
	tbuf[REQUIRED_VALUES_INDEX] = required_values;
	return required_values;
}

/*
 * Write the OSC address pattern and the OSC type tag, then update the
 * pointers to the memory used for the required values.
 */
unsigned int osc_start_message(void *buf, unsigned int maxlen, void *ibuf,
                               char *tbuf, const char *address,
                               const char *types)
{
	char *msg, *tmp;
	uint32_t *len;
	unsigned int ttag_start, bytes, i;

	len = (uint32_t *) buf;
	msg = buf + OSC_MESSAGE_LENGTH_SIZE;
	tmp = msg;
	memset(msg, 0, maxlen);
	for (i = 0; *address != 0; i++)
		tmp[i] = *address++;
	i = OSC_FIX_SIZE(i);
	tmp[i++] = ',';
	ttag_start = i;
	for (; *types != 0; i++)
		tmp[i] = *types++;
	tbuf[REQUIRED_VALUES_INDEX] = 0;
	index_osc_values(msg, ibuf, tbuf, ttag_start, OSC_FIX_SIZE(i));
	bytes = ((char **) ibuf)[MEM_FREE_INDEX] - msg;
	*len = ntohl(bytes);
	return bytes;
}

unsigned int osc_bundle_append_message(void *buf, void *ibuf, char *tbuf,
                                       const char *address, const char *types,
                                       unsigned int offset)
{
	char *msg, *tmp;
	uint32_t *len;
	int ttag_start, required_values, i;
	unsigned int bytes;

	len = (uint32_t *) (buf + offset);
	msg = ((char *) len) + OSC_MESSAGE_LENGTH_SIZE;
	tmp = msg;
	for (i = 0; *address != 0; i++)
		tmp[i] = *address++;
	i = OSC_FIX_SIZE(i);
	tmp[i++] = ',';
	ttag_start = i;
	for (; *types != 0; i++)
		tmp[i] = *types++;
	required_values = tbuf[REQUIRED_VALUES_INDEX];
	index_osc_values(msg, ibuf, tbuf, ttag_start, OSC_FIX_SIZE(i));
	/*
	 * Incremented here instead of from index_osc_values() because it
	 * is necessary for an OSC bundle with two or more OSC messages.
	 */
	tbuf[REQUIRED_VALUES_INDEX] += required_values;
	bytes = ((char **) ibuf)[MEM_FREE_INDEX] - msg;
	*len = ntohl(bytes);
	return bytes + OSC_MESSAGE_LENGTH_SIZE;
}

/*
 * The space required to store a string or a blob is variable. For example,
 * if the new received blob is bigger than the previous blob and there are
 * other data after the blob, it is necessary to reserve more space by
 * moving to right these data.
 */
int osc_maybe_reserve_space(void *oscbuf, void *ibuf, unsigned int index,
                            unsigned int size, int flags)
{
	uint32_t **data, *next;
	unsigned int old_size, i;
	int diff, shift;

	data = (uint32_t **) ibuf;
	/*
	 * If there are two or more OSC messages, the difference between the
	 * memory addresses of two adjacent values is not the value length.
	 * Note: a single message is also the last message.
	 */
	if (IS_LAST_MESSAGE(flags))
		old_size = ((char *) data[index + 1] - (char *) data[index]);
	else if (IS_BLOB(flags)) {
		old_size = htonl(data[index][0]);
		old_size = OSC_FIX_SIZE(old_size + 3);
	} else
		old_size = OSC_FIX_SIZE(strlen((char *) data[index]));
	/*
	 * Note: the pointer to the next value is equal to
	 * data[index + 1] if there is a single OSC message.
	 */
	next = (uint32_t *) ((char *) data[index] + old_size);
	diff = size - old_size;
	shift = diff / 4;
	if (shift != 0) {
		if (shift < 0)
			osc_move_data_left(next, data[MEM_FREE_INDEX], -shift);
		else
			osc_move_data_right(next, data[MEM_FREE_INDEX], shift);
		/* Update data pointer index. */
		for (i = index + 1; data[i] < data[MEM_FREE_INDEX]; i++)
			data[i] += shift;
		/* The last slot is the start of the free memory. */
		data[i] += shift;
		data[MEM_FREE_INDEX] = data[i];
	}
	/* Zero padding. */
	*(next + shift - 1) = 0;
	if (!(IS_SINGLE_MESSAGE(flags))
	    && (shift != 0)
	    && (osc_fix_length(oscbuf - OSC_MESSAGE_LENGTH_SIZE,
	                       data[index], data[MEM_FREE_INDEX], diff) != 0))
		fprintf(stderr, "ERROR: osc_fix_length() failed\n");
	return (char *) data[MEM_FREE_INDEX] - (char *) oscbuf;
}

static void osc_move_data_left(uint32_t *start, uint32_t *end, unsigned int n)
{
	uint32_t *curr;

	for (curr = start; curr < end; curr++)
		*(curr - n) = *curr;
}

static void osc_move_data_right(uint32_t *start, uint32_t *end, unsigned int n)
{
	uint32_t *curr, *arr_last;

	for (curr = end - 1, arr_last = start - 1; curr > arr_last; curr--)
		*(curr + n) = *curr;
}

/*
 * Called if the length of an OSC bundle with two or more messages is
 * changed (string or blob with different size).
 */
static int osc_fix_length(uint32_t *start, uint32_t *value, uint32_t *end, int diff)
{
	uint32_t *len, *p;
	int n;

	p = start;
	do {
		len = p;
		n = htonl(*len);
		p = (uint32_t *) ((char *) p + n + OSC_MESSAGE_LENGTH_SIZE);
		if (p > value) {
			*len = ntohl(n + diff);
			return 0;
		}
	} while (p < end);
	return -1;
}

struct osc_fds *osc_alloc_fds(void)
{
	struct osc_fds *o;

	o = (struct osc_fds *) malloc(sizeof(struct osc_fds));
	if (o != NULL) {
		FD_ZERO(&o->fds);
#ifndef WIN32
		o->servfd = -1;
#else
		o->newfd = 1;
#endif
		o->lastfd = -1;
		o->maxfd = 0;
		o->count = 0;
	}
	return o;
}

void osc_set_servfd(struct osc_fds *o, OSC_SOCKET servfd)
{
#ifndef WIN32
	o->servfd = servfd;
#else
	o->newfd = 1;
	SERVER_SOCKET(o) = servfd;
#endif
	o->maxfd = SERVER_FD(o);
	FD_SET(SERVER_SOCKET(o), &o->fds);
}

int osc_lastfd(struct osc_fds *o)
{
	return o->lastfd;
}

int osc_connections(struct osc_fds *o)
{
	return o->count;
}

void osc_close_connections(struct osc_fds *o)
{
	int i;

	for (i = 0; i <= o->maxfd; i++)
		if (FD_ISSET(GET_SOCKET(o,i), &o->fds) && i != SERVER_FD(o))
			CLOSE_SOCKET(GET_SOCKET(o,i));
	FD_ZERO(&o->fds);
	FD_SET(SERVER_SOCKET(o), &o->fds);
	o->maxfd = SERVER_FD(o);
	o->lastfd = -1;
	o->count = 0;
#ifdef WIN32
	o->newfd = 1;
#endif
}

int osc_next_fd_set(struct osc_fds *o, int curr)
{
	int i;

	if (curr > o->maxfd)
		return -1;
	for (i = curr + 1; i <= o->maxfd; i++) {
#ifndef WIN32
		if (i == SERVER_SOCKET(o)) continue;
#endif
		if (FD_ISSET(GET_SOCKET(o,i), &o->fds))
			return i;
	}
	return -1;
}

int osc_close_server(struct osc_fds *o)
{
	return CLOSE_SOCKET(SERVER_SOCKET(o));
}

/* Receiver used with stream-oriented protocols. */
int osc_recv(struct osc_fds *o, struct osc_address *addr, void *buf,
             unsigned int maxlen, int enc_flags, int flags)
{
	OSC_SOCKET client;
	int i, ret, remain, nbytes, nfds, is_slip, has_count_prefix;
	BUFFER_DATATYPE *data;
	fd_set fds, tmpfds;
	struct timeval now, *timeout;

	if (IS_NONBLOCKING(o, addr)) {
		now.tv_sec = now.tv_usec = 0;
		timeout = &now;
	} else {
		timeout = NULL;
	}
	is_slip = enc_flags & SLIP_ENCODING_FLAG;
	has_count_prefix = enc_flags & COUNT_PREFIX_FLAG;
	nbytes = 0;
	while(1) {
		fds = o->fds;
		nfds = o->maxfd + 1;
		if ((ret = select(nfds, &fds, NULL, NULL, timeout)) <= 0)
			return ret;
		if (FD_ISSET(SERVER_SOCKET(o), &fds) && AVAILABLE_FD(o)) {
			addr->saddr_len = sizeof(struct sockaddr_storage);
			client = accept(SERVER_SOCKET(o), (struct sockaddr *) addr->saddr,
			                &addr->saddr_len);
			if (client != OSC_INVALID_SOCKET) {
				FD_SET(client, &o->fds);
				o->count++;
#ifndef WIN32
				if (client > o->maxfd) o->maxfd = client;
#else
				o->fd_array[o->newfd] = client;
				if (o->newfd > o->maxfd) o->maxfd = o->newfd;
				o->newfd++;
#endif
			}
		}
		FD_LOOP(i, nfds, fds, o) {
			if (i != SERVER_FD(o)) {
				data = (BUFFER_DATATYPE *) buf;
				ret = recv(GET_SOCKET(o,i), data, maxlen, flags);
				if (ret == 0) {
					CLOSE_SOCKET(GET_SOCKET(o,i));
					FD_CLR(GET_SOCKET(o,i), &o->fds);
					if (o->lastfd == i)
						o->lastfd = -1;
					if (--o->count == 0) {
						o->maxfd = SERVER_FD(o);
#ifdef WIN32
						o->newfd = 1;
#endif
						break;
					} else {
						continue;
					}
				}
				o->lastfd = i;
				if (is_slip) {
					/* Serial Line IP */
					if (is_slip_msg((unsigned char *) data, ret))
						return ret;
					nbytes = maxlen;
					remain = nbytes - ret;
				} else if (has_count_prefix) {
					/*
					 * OSC 1.0 spec: length-count prefix on
					 * the start of the packet.
					 */
					nbytes = htonl(*((uint32_t *) data));
					if (nbytes > maxlen - 4) {
						nbytes = OSC_MSGTOOLONG;
						remain = maxlen - ret;
					} else {
						remain = nbytes + 4 - ret;
					}
				} else {
					return ret;
				}
				while (remain > 0) {
					FD_ZERO(&tmpfds);
					FD_SET(GET_SOCKET(o,i), &tmpfds);
					/* Timeout considered undefined. */
					now.tv_sec = now.tv_usec = 0;
					if ((select(i + 1, &tmpfds, NULL, NULL, &now) != -1)
					    && FD_ISSET(GET_SOCKET(o,i), &tmpfds)) {
						data += ret;
						ret = recv(GET_SOCKET(o,i), data, remain, flags);
						if (ret <= 0) {
							CLOSE_SOCKET(GET_SOCKET(o,i));
							if (o->lastfd == i)
								o->lastfd = -1;
							if (--o->count == 0)
								o->maxfd = SERVER_FD(o);
							nbytes = OSC_BADMSG;
							break;
						}
						remain -= ret;
						if (is_slip) {
							if (is_slip_msg((unsigned char *) data, ret))
								return nbytes - remain;
							else if (remain == 0)
								nbytes = OSC_MSGTOOLONG;
						}
					} else {
						nbytes = OSC_BADMSG;
						break;
					}
				}
				if (nbytes > 0)
					return nbytes;
			}
		}
		if (nbytes < 0)
			return nbytes;
		if (timeout == &now) {
			/* Timeout considered undefined after select() returns. */
			now.tv_sec = now.tv_usec = 0;
		}
	}
}

/*
 * SLIP (RFC 1055)
 * Transmission of IP datagrams over serial lines.
 */
unsigned int osc_slip_encode(const unsigned char *src, unsigned char *dest,
                             unsigned int len)
{
	unsigned int i, j;

	/* Double END character encoding. */
	dest[0] = SLIP_END;
	for (i = 0, j = 1; i < len; i++, j++) {
		switch (src[i]) {
		case SLIP_END:
			dest[j++] = SLIP_ESC;
			dest[j] = SLIP_ESC_END;
			break;
		case SLIP_ESC:
			dest[j++] = SLIP_ESC;
			dest[j] = SLIP_ESC_ESC;
			break;
		default:
			dest[j] = src[i];
		}
	}
	dest[j++] = SLIP_END;
	dest[j] = 0;
	return j;
}

unsigned int osc_slip_decode(unsigned char *buf, unsigned int maxlen)
{
	unsigned char *dest;
	unsigned int i, j;

	dest = buf;
	for (i = 0, j = 0; i < maxlen; i++) {
		switch (buf[i]) {
		case SLIP_END:
			if (i) return j;
			/* Skip initial END characters. */
			break;
		case SLIP_ESC:
			if (buf[++i] == SLIP_ESC_END)
				dest[j++] = SLIP_END;
			else if (buf[i] == SLIP_ESC_ESC)
				dest[j++] = SLIP_ESC;
			break;
		default:
			dest[j++] = buf[i];
		}
	}
	return j;
}

static int is_slip_msg(const unsigned char *buf, int len)
{
	int i = len - 1;

	while (i > 0)
		if (buf[i--] == SLIP_END)
			return 1;
	return 0;
}

int osc_getsock_broadcast(OSC_SOCKET s)
{
	socklen_t optlen;
	int ret;
	SOCKOPT_VALUE val;

	optlen = sizeof(SOCKOPT_VALUE);
	ret = getsockopt(s, SOL_SOCKET, SO_BROADCAST, &val, &optlen);
	return ret == 0 ? val : ret;
}

int osc_setsock_broadcast(OSC_SOCKET s, const struct addrinfo *info, int is_set)
{
	SOCKOPT_VALUE val = is_set;

	if (info->ai_socktype == SOCK_DGRAM && info->ai_family == AF_INET)
		return setsockopt(s, SOL_SOCKET, SO_BROADCAST, &val, sizeof(SOCKOPT_VALUE));
	return 0;
}

#ifndef WIN32
int osc_getsock_nonblock(OSC_SOCKET sockfd)
{
	int flags;

	if ((flags = fcntl(sockfd, F_GETFL, 0)) < 0)
		return 0;
	return flags & O_NONBLOCK;
}
#endif

#ifndef WIN32
int osc_setsock_nonblock(OSC_SOCKET sockfd, int is_nonblock)
{
	int flags;

	if ((flags = fcntl(sockfd, F_GETFL, 0)) < 0)
		return -1;
	flags = is_nonblock ? flags | O_NONBLOCK : flags & ~O_NONBLOCK;
	return fcntl(sockfd, F_SETFL, flags);
}
#else
int osc_setsock_nonblock(OSC_SOCKET s, int is_nonblock)
{
	unsigned long mode = is_nonblock;
	int err;

	err = ioctlsocket(s, FIONBIO, &mode);
	if (err != NO_ERROR)
		return -1;
	return 0;
}
#endif

int osc_setsock_reuseaddr(OSC_SOCKET s)
{
	SOCKOPT_VALUE yes = 1;
	return setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(SOCKOPT_VALUE));
}

unsigned int sizeof_socklen(void)
{
	return sizeof(socklen_t);
}

#ifdef WIN32
int initialize_winsock(void)
{
	WSADATA data;
	int res;

	res = WSAStartup(WSA_REQUIRED_VERSION, &data);
	return res;
}
#endif
