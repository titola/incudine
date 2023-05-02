;;; Copyright (c) 2015-2023 Tito Latini
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

#+win32
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:defcfun ("initialize_winsock" %initialize-winsock) :int)
  (cffi:defcfun ("WSACleanup" finalize-winsock) :int)

  (defun initialize-winsock ()
    (let ((res (%initialize-winsock)))
      (unless (zerop res)
        (network-error "WSAStartup failed: ~D\n" res))
      res))

  (initialize-winsock))

(cffi:defctype socket
  #-win32 :int
  #+(and win32 64-bit) :uint64
  #+(and win32 (not 64-bit)) :uint32)

#+linux
(define-constant MSG-NOSIGNAL #x4000)  ; Don't generate SIGPIPE.

(define-constant +default-msg-flags+ #+linux MSG-NOSIGNAL
                                     #-linux 0
  :documentation
    "Default flags for the C functions send, sendto, recv and recvfrom.
It defaults to MSG_NOSIGNAL (don't generate a SIGPIPE signal) on Linux.
See the manual pages of the C functions and bits/socket.h for details.")

(define-constant +socklen-type+
    (if (> (cffi:foreign-funcall "sizeof_socklen" :unsigned-int) 4)
        :uint64
        :uint32))

(cffi:defcstruct addrinfo
  (ai-flags :int)
  (ai-family :int)
  (ai-socktype :int)
  (ai-protocol :int)
  (ai-addrlen #.+socklen-type+)
  #-win32 (ai-addr :pointer)
  (ai-canonname :pointer)
  #+win32 (ai-addr :pointer)
  (ai-next :pointer))

(cffi:defcstruct address
  (info :pointer)               ; (struct addrinfo *)
  (sockaddr :pointer)           ; (struct sockaddr_storage *)
  (socklen #.+socklen-type+)
  #+win32
  (non-blocking :int))

#+win32
(defmacro cached-fd-array (osc-fds-ptr)
  `(cffi:inc-pointer ,osc-fds-ptr
     ;; int maxfd, lastfd, count, newfd
     #.(* 4 (cffi:foreign-type-size :int))))

(declaim (inline %send))
(cffi:defcfun ("send" %send) :int
  (sockfd socket)
  (buf :pointer)
  (len :unsigned-int)
  (flags :int))

(declaim (inline %sendto))
(cffi:defcfun ("sendto" %sendto) :int
  (sockfd socket)
  (buf :pointer)
  (len :unsigned-int)
  (flags :int)
  (dest-addr :pointer)
  (addrlen #.+socklen-type+))

(declaim (inline %recvfrom))
(cffi:defcfun ("recvfrom" %recvfrom) :int
  (sockfd socket)
  (buf :pointer)
  (len :unsigned-int)
  (flags :int)
  (sockaddr :pointer)
  (socklen :pointer))

(cffi:defcfun (#-win32 "close" #+win32 "closesocket" close-socket) :int
  (sockfd socket))

(declaim (inline %osc-recv))
(cffi:defcfun ("osc_recv" %osc-recv) :int
  (fds :pointer)
  (address :pointer)
  (buf :pointer)
  (maxlen :unsigned-int)
  (enc-flags :int)
  (flags :int))

(cffi:defcfun ("osc_address_new" new-address) :int
  (addr :pointer)
  (host :string)
  (port :unsigned-int)
  (datagram-p :boolean)
  (input-p :boolean)
  (hints-flags :int))

(cffi:defcfun ("osc_address_free" free-address) :void
  (addr :pointer))

(declaim (inline %check-pattern))
(cffi:defcfun ("check_osc_pattern" %check-pattern) :boolean
  (buffer :pointer)
  (address :string)
  (types :string))

(declaim (inline %index-values))
(cffi:defcfun ("index_osc_values" %index-values) :int
  (buf :pointer)
  (ibuf :pointer)
  (tbuf :pointer)
  (types-start :unsigned-int)
  (data-start :unsigned-int))

(declaim (inline %index-bundle-values))
(cffi:defcfun ("index_osc_bundle_values" %index-bundle-values) :int
  (buf :pointer)
  (ibuf :pointer)
  (tbuf :pointer)
  (size :unsigned-int)
  (swap :boolean))

#+little-endian
(progn
  (declaim (inline %index-values-le))
  (cffi:defcfun ("index_osc_values_le" %index-values-le) :int
    (buf :pointer)
    (ibuf :pointer)
    (tbuf :pointer)
    (types-start :unsigned-int)
    (data-start :unsigned-int)))

(declaim (inline %start-message))
(cffi:defcfun ("osc_start_message" %start-message) :unsigned-int
  (buffer :pointer)
  (maxlen :unsigned-int)
  (ibuf :pointer)
  (tbuf :pointer)
  (address :string)
  (types :string))

(declaim (inline %append-message))
(cffi:defcfun ("osc_bundle_append_message" %append-message) :unsigned-int
  (buffer :pointer)
  (ibuf :pointer)
  (tbuf :pointer)
  (address :string)
  (types :string)
  (offset :unsigned-int))

(declaim (inline osc-reserve-space))
(cffi:defcfun ("osc_maybe_reserve_space" %maybe-reserve-space) :int
  (oscbuf :pointer)
  (ibuf :pointer)
  (index :unsigned-int)
  (data-size :unsigned-int)
  (flags :int))

(declaim (inline %alloc-fds))
(cffi:defcfun ("osc_alloc_fds" %alloc-fds) :pointer)

(declaim (inline set-server-fd))
(cffi:defcfun ("osc_set_servfd" set-server-fd) :void
  (fds :pointer)
  (servfd socket))

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
  (sockfd socket))

(declaim (inline setsock-broadcast))
(cffi:defcfun ("osc_setsock_broadcast" setsock-broadcast) :int
  (sockfd socket)
  (info :pointer)
  (enable-p :boolean))

#-win32
(cffi:defcfun ("osc_getsock_nonblock" getsock-nonblock) :boolean
  (sockfd socket))

(cffi:defcfun ("osc_setsock_nonblock" setsock-nonblock) :int
  (sockfd socket)
  (nonblock-p :boolean))

(cffi:defcfun ("osc_setsock_reuseaddr" setsock-reuseaddr) :int
  (sockfd socket))
