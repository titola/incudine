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

#include "mouse.h"

int mouse_init(void)
{
        if (mouse_status == MOUSE_NOINIT || disp == NULL) {
                XInitThreads();
                if ((disp = XOpenDisplay(NULL)) == NULL)
                        return -1;
                mouse_status = MOUSE_STOPPED;
                req_time.tv_sec = 0;
                req_time.tv_nsec = MOUSE_LOOP_WAIT_NSEC;
                win = DefaultRootWindow(disp);
                XGetWindowAttributes(disp, win, &win_attrib);
                width = 1.0 / (SAMPLE)win_attrib.width;
                height = 1.0 / (SAMPLE)win_attrib.height;
                return 0;
        }
        return -1;
}

int mouse_loop_start(struct mouse_event *ev)
{
        mouse_status = MOUSE_STARTED;
        while (mouse_status) {
                XQueryPointer(disp, win, &root_ret, &child_ret, &root_x_ret,
                              &root_y_ret, &win_x_ret, &win_y_ret, &mask_ret);
                ev->x = (SAMPLE)win_x_ret * width;
                ev->y = 1.0 - ((SAMPLE)win_y_ret * height);
                ev->button = (int)(mask_ret & Button1Mask) > 0;
                nanosleep(&req_time, &rem_time);
        }
        return 0;
}

int mouse_loop_stop(void)
{
        mouse_status = MOUSE_STOPPED;
        return 0;
}

int get_mouse_status(void)
{
        return mouse_status;
}
