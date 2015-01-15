;;; Copyright (c) 2015 Tito Latini
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :incudine.osc)

#+linux
(define-constant MSG-NOSIGNAL #x4000)

(define-constant +socklen-type+
    (if (> (cffi:foreign-funcall "sizeof_socklen" :unsigned-int) 4)
        :uint64
        :uint32))

(cffi:defcfun ("osc_address_new" new-address) :int
  (addr :pointer)
  (host :string)
  (port :unsigned-int)
  (datagram-p :boolean)
  (input-p :boolean))

(cffi:defcfun ("osc_address_free" free-address) :void
  (addr :pointer))

(declaim (inline %check-pattern))
(cffi:defcfun ("check_osc_pattern" %check-pattern) :boolean
  (buffer :pointer)
  (address :string)
  (types :string))

(declaim (inline %index-values))
(cffi:defcfun ("index_osc_values" %index-values) :void
  (buf :pointer)
  (ibuf :pointer)
  (tbuf :pointer)
  (types-start :unsigned-int)
  (data-start :unsigned-int))

#+little-endian
(progn
  (declaim (inline %index-values-le))
  (cffi:defcfun ("index_osc_values_le" %index-values-le) :void
    (buf :pointer)
    (ibuf :pointer)
    (tbuf :pointer)
    (types-start :unsigned-int)
    (data-start :unsigned-int)))

(cffi:defcfun ("osc_start_message" %start-message) :unsigned-int
  (buffer :pointer)
  (bufsize :unsigned-int)
  (ibuf :pointer)
  (tbuf :pointer)
  (address :string)
  (types :string))

(declaim (inline osc-reserve-space))
(cffi:defcfun ("osc_maybe_reserve_space" %maybe-reserve-space) :int
  (oscbuf :pointer)
  (ibuf :pointer)
  (index :unsigned-int)
  (data-size :unsigned-int))

(declaim (inline %alloc-fds))
(cffi:defcfun ("osc_alloc_fds" %alloc-fds) :pointer)

(declaim (inline set-server-fd))
(cffi:defcfun ("osc_set_servfd" set-server-fd) :void
  (fds :pointer)
  (servfd :int))

(declaim (inline %close-connections))
(cffi:defcfun ("osc_close_connections" %close-connections) :void
  (fds :pointer))

(declaim (inline close-server))
(cffi:defcfun ("osc_close_server" close-server) :int
  (fds :pointer))

(declaim (inline %slip-encode))
(cffi:defcfun ("osc_slip_encode" %slip-encode) :unsigned-int
  (src :pointer)
  (dest :pointer)
  (len :unsigned-int))

(declaim (inline %slip-decode))
(cffi:defcfun ("osc_slip_decode" %slip-decode) :unsigned-int
  (buf :pointer)
  (maxlen :unsigned-int))

(cffi:defcfun ("osc_getsock_broadcast" getsock-broadcast) :int
  (sockfd :int))

(declaim (inline setsock-broadcast))
(cffi:defcfun ("osc_setsock_broadcast" setsock-broadcast) :int
  (sockfd :int)
  (info :pointer)
  (enable-p :boolean))

(cffi:defcfun ("osc_getsock_nonblock" getsock-nonblock) :boolean
  (sockfd :int))

(cffi:defcfun ("osc_setsock_nonblock" setsock-nonblock) :int
  (sockfd :int)
  (nonblock-p :boolean))

(cffi:defcfun ("osc_setsock_reuseaddr" setsock-reuseaddr) :int
  (sockfd :int))

(cffi:defcfun ("osc_strsize" string-size) :unsigned-int
  (string :string))
