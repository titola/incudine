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

#ifndef __MOUSE_H
#define __MOUSE_H

#include <unistd.h>
#include <X11/Intrinsic.h>
#include <time.h>
#include "common.h"

struct mouse_event {
        SAMPLE x, y;
        int button;
};

enum {
        MOUSE_NOINIT = -1,
        MOUSE_STOPPED,
        MOUSE_STARTED
};

#define MOUSE_LOOP_WAIT_NSEC  (17000000);

static Display *disp = NULL;
static Window win, root_ret, child_ret;
static XWindowAttributes win_attrib;
static int root_x_ret, root_y_ret;
static int win_x_ret, win_y_ret;
static int mouse_status = MOUSE_NOINIT;
static unsigned int mask_ret;
static SAMPLE xmult, ymult;
static struct timespec req_time, rem_time;

int mouse_init(void);
int mouse_loop_start(struct mouse_event *ev);
int mouse_loop_stop(void);
int get_mouse_status(void);

#endif  /* __MOUSE_H */
