;;; Copyright (c) 2013-2014 Tito Latini
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :cl-user)

(defpackage lv2
  (:use #:cl)
  (:export
   #:handle
   #:feature
   #:descriptor
   #:event
   #:event-buffer
   #:event-feature))

(defpackage lilv
  (:use #:cl)
  (:export
   #:*world*
   #:*uri-audio-port*
   #:*uri-control-port*
   #:*uri-input-port*
   #:*uri-output-port*
   #:*uri-event-port*
   #:*uri-midi-port*
   ;; types
   #:instance-impl
   #:world
   #:instance
   ;; functions
   #:free-p
   #:free
   #:lv2-init
   #:plugin-pointer
   #:uri-to-path
   #:new-uri
   #:new-string
   #:new-int
   #:new-float
   #:new-bool
   #:node-free
   #:node-duplicate
   #:node-equals
   #:node-get-turtle-token
   #:node-is-uri
   #:node-as-uri
   #:node-is-blank
   #:node-as-blank
   #:node-is-literal
   #:node-is-string
   #:node-as-string
   #:node-is-float
   #:node-as-float
   #:node-is-int
   #:node-as-int
   #:node-is-bool
   #:node-as-bool
   #:plugin-classes-free
   #:plugin-classes-size
   #:plugin-classes-begin
   #:plugin-classes-get
   #:plugin-classes-next
   #:plugin-classes-is-end
   #:plugin-classes-get-by-uri
   #:scale-points-free
   #:scale-points-size
   #:scale-points-begin
   #:scale-points-get
   #:scale-points-next
   #:scale-points-is-end
   #:uis-free
   #:uis-size
   #:uis-begin
   #:uis-get
   #:uis-next
   #:uis-is-end
   #:uis-get-by-uri
   #:nodes-free
   #:nodes-size
   #:nodes-begin
   #:nodes-get
   #:nodes-next
   #:nodes-is-end
   #:nodes-get-first
   #:nodes-contains
   #:nodes-merge
   #:plugins-size
   #:plugins-begin
   #:plugins-get
   #:plugins-next
   #:plugins-is-end
   #:plugins-get-by-uri
   #:init-world
   #:world-new
   #:world-set-option
   #:world-load-all
   #:world-load-bundle
   #:world-load-resource
   #:world-get-plugin-class
   #:world-get-plugin-classes
   #:world-get-all-plugins
   #:world-find-nodes
   #:world-get
   #:world-ask
   #:plugin-verify
   #:plugin-get-uri
   #:plugin-get-bundle-uri
   #:plugin-get-data-uris
   #:plugin-get-library-uri
   #:plugin-get-name
   #:plugin-get-class
   #:plugin-get-value
   #:plugin-has-feature
   #:plugin-get-supported-features
   #:plugin-get-required-features
   #:plugin-get-optional-features
   #:plugin-has-extension-data
   #:plugin-get-extension-data
   #:plugin-get-num-ports
   #:plugin-get-port-ranges-float
   #:plugin-get-num-ports-of-class
   #:plugin-has-latency
   #:plugin-get-latency-port-index
   #:plugin-get-port-by-index
   #:plugin-get-port-by-symbol
   #:plugin-get-port-by-designation
   #:plugin-get-project
   #:plugin-get-author-name
   #:plugin-get-author-email
   #:plugin-get-author-homepage
   #:plugin-is-replaced
   #:plugin-write-description
   #:plugin-write-manifest-entry
   #:plugin-get-related
   #:port-get-value
   #:port-get
   #:port-get-properties
   #:port-has-property
   #:port-supports-event
   #:port-get-index
   #:port-get-symbol
   #:port-get-name
   #:port-get-classes
   #:port-is-a
   #:port-get-range
   #:port-get-scale-points
   #:state-new-from-world
   #:state-new-from-file
   #:state-new-from-string
   #:state-new-from-instance
   #:state-free
   #:state-equals
   #:state-get-num-properties
   #:state-get-plugin-uri
   #:state-get-label
   #:state-set-label
   #:state-restore
   #:state-save
   #:state-to-string
   #:scale-point-get-label
   #:scale-point-get-value
   #:plugin-class-get-parent-uri
   #:plugin-class-get-uri
   #:plugin-class-get-label
   #:plugin-class-get-children
   #:plugin-instantiate
   #:instance-get-descriptor
   #:instance-get-handle
   #:instance-get-uri
   #:instance-connect-port
   #:instance-activate
   #:instance-deactivate
   #:instance-impl-slot-value
   #:instance-slot-value
   #:connect-port
   #:descriptor-slot-value
   #:plugin-get-uis
   #:ui-get-uri
   #:ui-get-classes
   #:ui-is-a
   #:ui-is-supported
   #:ui-get-bundle-uri
   #:ui-get-binary-uri))
