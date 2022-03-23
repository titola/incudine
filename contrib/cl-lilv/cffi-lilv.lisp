;;; Copyright (c) 2013-2022 Tito Latini
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

(in-package :lilv)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *standard-optimize-settings*
    '(optimize speed (safety 0)))

  (cffi:define-foreign-library lilv
    (:darwin "liblilv-0.dylib")
    (:unix "liblilv-0.so")
    (:cygwin "cyglilv-0.dll")
    (t (:default "liblilv-0")))

  (defun load-lilv-library ()
    (cffi:use-foreign-library lilv))

  (unless (cffi:foreign-library-loaded-p 'lilv)
    (load-lilv-library)))

(defstruct (pointer-wrap (:copier nil))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer))

(defstruct (world (:include pointer-wrap) (:copier nil))
  (features (cffi:null-pointer) :type cffi:foreign-pointer))

(defstruct (instance (:include pointer-wrap)
                     (:constructor make-lilv-instance)
                     (:copier nil))
  (active-p nil :type boolean))

(cffi:define-foreign-type world-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser world))

(cffi:define-foreign-type instance-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser instance))

(declaim (inline finalize))
(defun finalize (obj function)
  #+sbcl (sb-ext:finalize obj function :dont-save t)
  #-sbcl (tg:finalize obj function))

(declaim (inline cancel-finalization))
(defun cancel-finalization (obj)
  #+sbcl (sb-ext:cancel-finalization obj)
  #-sbcl (tg:cancel-finalization obj))

(defmethod cffi:translate-from-foreign (ptr (type world-type))
  (let* ((features (lv2::make-lv2-features))
         (obj (make-world :pointer ptr :features features)))
    (finalize obj
      (lambda ()
        (lv2::free-features)
        (cffi:foreign-free features)
        (world-free ptr)))))

(defmethod cffi:translate-to-foreign (handle (type world-type))
  (slot-value handle 'pointer))

(defmethod cffi:translate-from-foreign (ptr (type instance-type))
  (let ((obj (make-lilv-instance :pointer ptr)))
    (finalize obj (lambda ()
                    (instance-impl-deactivate ptr)
                    (instance-free ptr)))
    obj))

(defmethod cffi:translate-to-foreign (handle (type instance-type))
  (slot-value handle 'pointer))

(defctype node-ptr :pointer)

(defcstruct instance-impl
  (lv2:descriptor :pointer)
  (lv2:handle     :pointer)
  (pimpl          :pointer))

;;; lilv_uri_to_path() is deprecated.
(when (cffi:foreign-symbol-pointer "lilv_uri_to_path")
  (defcfun ("lilv_uri_to_path" uri-to-path) :string (uri :string))
  (define-compiler-macro uri-to-path (&whole form uri)
    (declare (ignore uri))
    (warn "LILV:URI-TO-PATH is deprecated, use LILV:FILE-URI-PARSE instead.")
    form))

(defcfun ("lilv_file_uri_parse" file-uri-parse) :pointer
  (uri :string)
  (hostname :pointer))

(defcfun ("lilv_new_uri" new-uri) node-ptr
  (world world)
  (uri   :string))

(defcfun ("lilv_new_string" new-string) node-ptr
  (world world)
  (str   :string))

(defcfun ("lilv_new_int" new-int) node-ptr
  (world world)
  (val   :int))

(defcfun ("lilv_new_float" new-float) node-ptr
  (world world)
  (val   :float))

(defcfun ("lilv_new_bool" new-bool) node-ptr
  (world world)
  (val   :boolean))

(defcfun ("lilv_node_free" node-free) :void
  (val node-ptr))

(defcfun ("lilv_node_duplicate" node-duplicate) node-ptr
  (val node-ptr))

(defcfun ("lilv_node_equals" node-equals) :boolean
  (value node-ptr)
  (other node-ptr))

(defcfun ("lilv_node_get_turtle_token" node-get-turtle-token) :string
  (value node-ptr))

(defcfun ("lilv_node_is_uri" node-is-uri) :boolean
  (value node-ptr))

(defcfun ("lilv_node_as_uri" node-as-uri) :string
  (value node-ptr))

(defcfun ("lilv_node_is_blank" node-is-blank) :boolean
  (value node-ptr))

(defcfun ("lilv_node_as_blank" node-as-blank) :string
  (value node-ptr))

(defcfun ("lilv_node_is_literal" node-is-literal) :boolean
  (value node-ptr))

(defcfun ("lilv_node_is_string" node-is-string) :boolean
  (value node-ptr))

(defcfun ("lilv_node_as_string" node-as-string) :string
  (value node-ptr))

(defcfun ("lilv_node_get_path" node-get-path) :pointer
  (value node-ptr)
  (hostname :pointer))

(defcfun ("lilv_node_is_float" node-is-float) :boolean
  (value node-ptr))

(defcfun ("lilv_node_as_float" node-as-float) :float
  (value node-ptr))

(defcfun ("lilv_node_is_int" node-is-int) :boolean
  (value node-ptr))

(defcfun ("lilv_node_as_int" node-as-int) :int
  (value node-ptr))

(defcfun ("lilv_node_is_bool" node-is-bool) :boolean
  (value node-ptr))

(defcfun ("lilv_node_as_bool" node-as-bool) :boolean
  (value node-ptr))

(defcfun ("lilv_plugin_classes_free" plugin-classes-free) :void
  (collection :pointer))

(defcfun ("lilv_plugin_classes_size" plugin-classes-size) :unsigned-int
  (collection :pointer))

(defcfun ("lilv_plugin_classes_begin" plugin-classes-begin) :pointer
  (collection :pointer))

(defcfun ("lilv_plugin_classes_get" plugin-classes-get) :pointer
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_plugin_classes_next" plugin-classes-next) :pointer
  (collection :pointer)
  (i :pointer))

(defcfun ("lilv_plugin_classes_is_end" plugin-classes-is-end) :boolean
  (collection :pointer)
  (i :pointer))

(defcfun ("lilv_plugin_classes_get_by_uri" plugin-classes-get-by-uri) :pointer
  (classes :pointer)
  (uri     node-ptr))

(defcfun ("lilv_scale_points_free" scale-points-free) :void
  (collection :pointer))

(defcfun ("lilv_scale_points_size" scale-points-size) :unsigned-int
  (collection :pointer))

(defcfun ("lilv_scale_points_begin" scale-points-begin) :pointer
  (collection :pointer))

(defcfun ("lilv_scale_points_get" scale-points-get) :pointer
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_scale_points_next" scale-points-next) :pointer
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_scale_points_is_end" scale-points-is-end) :boolean
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_uis_free" uis-free) :void
  (collection :pointer))

(defcfun ("lilv_uis_size" uis-size) :unsigned-int
  (collection :pointer))

(defcfun ("lilv_uis_begin" uis-begin) :pointer
  (collection :pointer))

(defcfun ("lilv_uis_get" uis-get) :pointer
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_uis_next" uis-next) :pointer
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_uis_is_end" uis-is-end) :boolean
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_uis_get_by_uri" uis-get-by-uri) :pointer
  (uis :pointer)
  (uri node-ptr))

(defcfun ("lilv_nodes_free" nodes-free) :void
  (collection :pointer))

(defcfun ("lilv_nodes_size" nodes-size) :unsigned-int
  (collection :pointer))

(defcfun ("lilv_nodes_begin" nodes-begin) :pointer
  (collection :pointer))

(defcfun ("lilv_nodes_get" nodes-get) node-ptr
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_nodes_next" nodes-next) :pointer
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_nodes_is_end" nodes-is-end) :boolean
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_nodes_get_first" nodes-get-first) node-ptr
  (collection :pointer))

(defcfun ("lilv_nodes_contains" nodes-contains) :boolean
  (values :pointer)
  (value  node-ptr))

(defcfun ("lilv_nodes_merge" nodes-merge) :pointer
  (a :pointer)
  (b :pointer))

(defcfun ("lilv_plugins_size" plugins-size) :unsigned-int
  (collection :pointer))

(defcfun ("lilv_plugins_begin" plugins-begin) :pointer
  (collection :pointer))

(defcfun ("lilv_plugins_get" plugins-get) :pointer
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_plugins_next" plugins-next) :pointer
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_plugins_is_end" plugins-is-end) :boolean
  (collection :pointer)
  (i          :pointer))

(defcfun ("lilv_plugins_get_by_uri" plugins-get-by-uri) :pointer
  (plugins :pointer)
  (uri     node-ptr))

(defcfun ("lilv_world_new" world-new) world)

(defcfun ("lilv_world_set_option" world-set-option) :void
  (world world)
  (uri   :string)
  (value node-ptr))

(defcfun ("lilv_world_free" world-free) :void
  (world :pointer))

(defcfun ("lilv_world_load_all" world-load-all) :void
  (world world))

(defcfun ("lilv_world_load_bundle" world-load-bundle) :void
  (world      world)
  (bundle-uri node-ptr))

(defcfun ("lilv_world_load_resource" world-load-resource) :int
  (world    world)
  (resource node-ptr))

(defcfun ("lilv_world_get_plugin_class" world-get-plugin-class) :pointer
  (world world))

(defcfun ("lilv_world_get_plugin_classes" world-get-plugin-classes) :pointer
  (world world))

(defcfun ("lilv_world_get_all_plugins" world-get-all-plugins) :pointer
  (world world))

(defcfun ("lilv_world_find_nodes" world-find-nodes) :pointer
  (world     world)
  (subject   node-ptr)
  (predicate node-ptr)
  (object    node-ptr))

(defcfun ("lilv_world_get" world-get) node-ptr
  (world     world)
  (subject   node-ptr)
  (predicate node-ptr)
  (object    node-ptr))

(defcfun ("lilv_world_ask" world-ask) :boolean
  (world     world)
  (subject   node-ptr)
  (predicate node-ptr)
  (object    node-ptr))

(defcfun ("lilv_plugin_verify" plugin-verify) :boolean
  (plugin :pointer))

(defcfun ("lilv_plugin_get_uri" plugin-get-uri) node-ptr
  (plugin :pointer))

(defcfun ("lilv_plugin_get_bundle_uri" plugin-get-bundle-uri) node-ptr
  (plugin :pointer))

(defcfun ("lilv_plugin_get_data_uris" plugin-get-data-uris) :pointer
  (plugin :pointer))

(defcfun ("lilv_plugin_get_library_uri" plugin-get-library-uri) node-ptr
  (plugin :pointer))

(defcfun ("lilv_plugin_get_name" plugin-get-name) node-ptr
  (plugin :pointer))

(defcfun ("lilv_plugin_get_class" plugin-get-class) :pointer
  (plugin :pointer))

(defcfun ("lilv_plugin_get_value" plugin-get-value) :pointer
  (p         :pointer)
  (predicate node-ptr))

(defcfun ("lilv_plugin_has_feature" plugin-has-feature) :boolean
  (p           :pointer)
  (feature-uri node-ptr))

(defcfun ("lilv_plugin_get_supported_features" plugin-get-supported-features) :pointer
  (p :pointer))

(defcfun ("lilv_plugin_get_required_features" plugin-get-required-features) :pointer
  (p :pointer))

(defcfun ("lilv_plugin_get_optional_features" plugin-get-optional-features) :pointer
  (p :pointer))

(defcfun ("lilv_plugin_has_extension_data" plugin-has-extension-data) :boolean
  (p   :pointer)
  (uri node-ptr))

(defcfun ("lilv_plugin_get_extension_data" plugin-get-extension-data) :pointer
  (p :pointer))

(defcfun ("lilv_plugin_get_num_ports" plugin-get-num-ports) :uint32
  (p :pointer))

(defcfun ("lilv_plugin_get_port_ranges_float" plugin-get-port-ranges-float) :void
  (p          :pointer)
  (min-values :pointer)
  (max-values :pointer)
  (def-values :pointer))

(defcfun ("lilv_plugin_get_num_ports_of_class" plugin-get-num-ports-of-class) :uint32
  (p       :pointer)
  (class-1 node-ptr)
  &rest)

(defcfun ("lilv_plugin_has_latency" plugin-has-latency) :boolean
  (p :pointer))

(defcfun ("lilv_plugin_get_latency_port_index" plugin-get-latency-port-index) :uint32
  (p :pointer))

(defcfun ("lilv_plugin_get_port_by_index" plugin-get-port-by-index) :pointer
  (plugin :pointer)
  (index  :uint32))

(defcfun ("lilv_plugin_get_port_by_symbol" plugin-get-port-by-symbol) :pointer
  (plugin :pointer)
  (symbol node-ptr))

(defcfun ("lilv_plugin_get_port_by_designation" plugin-get-port-by-designation) :pointer
  (plugin      :pointer)
  (port-class  node-ptr)
  (designation node-ptr))

(defcfun ("lilv_plugin_get_project" plugin-get-project) node-ptr
  (plugin :pointer))

(defcfun ("lilv_plugin_get_author_name" plugin-get-author-name) node-ptr
  (plugin :pointer))

(defcfun ("lilv_plugin_get_author_email" plugin-get-author-email) node-ptr
  (plugin :pointer))

(defcfun ("lilv_plugin_get_author_homepage" plugin-get-author-homepage) node-ptr
  (plugin :pointer))

(defcfun ("lilv_plugin_is_replaced" plugin-is-replaced) :boolean
  (plugin :pointer))

(defcfun ("lilv_plugin_write_description" plugin-write-description) :void
  (world       world)
  (plugin      :pointer)
  (base-uri    node-ptr)
  (plugin-file :pointer))

(defcfun ("lilv_plugin_write_manifest_entry" plugin-write-manifest-entry) :void
  (world            world)
  (plugin           :pointer)
  (base-uri         node-ptr)
  (manifest-file    :pointer)
  (plugin-file-path :string))

(defcfun ("lilv_plugin_get_related" plugin-get-related) :pointer
  (plugin :pointer)
  (type   node-ptr))

(defcfun ("lilv_port_get_value" port-get-value) :pointer
  (plugin    :pointer)
  (port      :pointer)
  (predicate node-ptr))

(defcfun ("lilv_port_get" port-get) node-ptr
  (plugin    :pointer)
  (port      :pointer)
  (predicate node-ptr))

(defcfun ("lilv_port_get_properties" port-get-properties) :pointer
  (plugin :pointer)
  (port   :pointer))

(defcfun ("lilv_port_has_property" port-has-property) :boolean
  (p            :pointer)
  (port         :pointer)
  (property-uri node-ptr))

(defcfun ("lilv_port_supports_event" port-supports-event) :boolean
  (p          :pointer)
  (port       :pointer)
  (event-type node-ptr))

(defcfun ("lilv_port_get_index" port-get-index) :uint32
  (plugin :pointer)
  (port   :pointer))

(defcfun ("lilv_port_get_symbol" port-get-symbol) node-ptr
  (plugin :pointer)
  (port   :pointer))

(defcfun ("lilv_port_get_name" port-get-name) node-ptr
  (plugin :pointer)
  (port   :pointer))

(defcfun ("lilv_port_get_classes" port-get-classes) :pointer
  (plugin :pointer)
  (port   :pointer))

(defcfun ("lilv_port_is_a" port-is-a) :boolean
  (plugin     :pointer)
  (port       :pointer)
  (port-class node-ptr))

(defcfun ("lilv_port_get_range" port-get-range) :void
  (plugin :pointer)
  (port   :pointer)
  (deflt  :pointer)
  (min    :pointer)
  (max    :pointer))

(defcfun ("lilv_port_get_scale_points" port-get-scale-points) :pointer
  (plugin :pointer)
  (port   :pointer))

(defcfun ("lilv_state_new_from_world" state-new-from-world) :pointer
  (world   world)
  (map     :pointer)
  (subject node-ptr))

(defcfun ("lilv_state_new_from_file" state-new-from-file) :pointer
  (world   world)
  (map     :pointer)
  (subject node-ptr)
  (path    :string))

(defcfun ("lilv_state_new_from_string" state-new-from-string) :pointer
  (world world)
  (map   :pointer)
  (str   :string))

(defcfun ("lilv_state_new_from_instance" state-new-from-instance) :pointer
  (plugin    :pointer)
  (instance  instance)
  (map       :pointer)
  (file-dir  :string)
  (copy-dir  :string)
  (link-dir  :string)
  (save-dir  :string)
  (get-value :pointer)
  (user-data :pointer)
  (flags     :uint32)
  (features  :pointer))

(defcfun ("lilv_state_free" state-free) :void
  (state :pointer))

(defcfun ("lilv_state_equals" state-equals) :boolean
  (a :pointer)
  (b :pointer))

(defcfun ("lilv_state_get_num_properties" state-get-num-properties) :unsigned-int
  (state :pointer))

(defcfun ("lilv_state_get_plugin_uri" state-get-plugin-uri) node-ptr
  (state :pointer))

(defcfun ("lilv_state_get_label" state-get-label) :string
  (state :pointer))

(defcfun ("lilv_state_set_label" state-set-label) :void
  (state :pointer)
  (label :string))

(defcfun ("lilv_state_restore" state-restore) :void
  (state     :pointer)
  (instance  instance)
  (set-value :pointer)
  (user-data :pointer)
  (flags     :uint32)
  (features  :pointer))

(defcfun ("lilv_state_save" state-save) :int
  (world    world)
  (map      :pointer)
  (unmap    :pointer)
  (state    :pointer)
  (uri      :string)
  (dir      :string)
  (filename :string))

(defcfun ("lilv_state_to_string" state-to-string) :string
  (world    world)
  (map      :pointer)
  (unmap    :pointer)
  (state    :pointer)
  (uri      :string)
  (base-uri :string))

(defcfun ("lilv_scale_point_get_label" scale-point-get-label) node-ptr
  (point :pointer))

(defcfun ("lilv_scale_point_get_value" scale-point-get-value) node-ptr
  (point :pointer))

(defcfun ("lilv_plugin_class_get_parent_uri" plugin-class-get-parent-uri) node-ptr
  (plugin-class :pointer))

(defcfun ("lilv_plugin_class_get_uri" plugin-class-get-uri) node-ptr
  (plugin-class :pointer))

(defcfun ("lilv_plugin_class_get_label" plugin-class-get-label) node-ptr
  (plugin-class :pointer))

(defcfun ("lilv_plugin_class_get_children" plugin-class-get-children) :pointer
  (plugin-class :pointer))

(defcfun ("lilv_plugin_instantiate" plugin-instantiate) instance
  (plugin      :pointer)
  (sample-rate :double)
  (features    :pointer))

(defcfun ("lilv_instance_free" instance-free) :void
  (instance :pointer))

(defcfun ("lilv_plugin_get_uis" plugin-get-uis) :pointer
  (plugin :pointer))

(defcfun ("lilv_ui_get_uri" ui-get-uri) node-ptr
  (ui :pointer))

(defcfun ("lilv_ui_get_classes" ui-get-classes) :pointer
  (ui :pointer))

(defcfun ("lilv_ui_is_a" ui-is-a) :boolean
  (ui        :pointer)
  (class-uri node-ptr))

(defcfun ("lilv_ui_is_supported" ui-is-supported) :unsigned-int
  (ui             :pointer)
  (supported-func :pointer)
  (container-type node-ptr)
  (ui-type        node-ptr))

(defcfun ("lilv_ui_get_bundle_uri" ui-get-bundle-uri) node-ptr
  (ui :pointer))

(defcfun ("lilv_ui_get_binary_uri" ui-get-binary-uri) node-ptr
  (ui :pointer))

(defmacro instance-impl-slot-value (instance-ptr slot-name)
  `(cffi:foreign-slot-value ,instance-ptr '(:struct instance-impl) ,slot-name))

(defmacro instance-slot-value (instance slot-name)
  `(instance-impl-slot-value (instance-pointer ,instance) ,slot-name))

(declaim (inline instance-get-descriptor))
(defun instance-get-descriptor (instance)
  (instance-slot-value instance 'lv2:descriptor))

(declaim (inline instance-get-handle))
(defun instance-get-handle (instance)
  (instance-slot-value instance 'lv2:handle))

(defmacro descriptor-slot-value (pointer slot-name)
  `(cffi:foreign-slot-value ,pointer '(:struct lv2:descriptor) ,slot-name))

(declaim (inline instance-get-uri))
(defun instance-get-uri (instance)
  (descriptor-slot-value (instance-get-descriptor instance) 'lv2::uri))

(defun instance-get-extension-data (instance uri)
  (let ((fn (descriptor-slot-value
              (instance-get-descriptor instance) 'lv2::extension-data)))
    (if (cffi:null-pointer-p fn)
        (cffi:null-pointer)
        (cffi:foreign-funcall-pointer fn () :string uri :pointer))))

(declaim (inline connect-port))
(defun connect-port (callback handle index data-location)
  (cffi:foreign-funcall-pointer callback () :pointer handle :uint32 index
                                :pointer data-location :void))

(declaim (inline instance-connect-port))
(defun instance-connect-port (instance port-index data-location)
  (let ((cb (descriptor-slot-value (instance-get-descriptor instance)
                                   'lv2::connect-port)))
    (unless (cffi:null-pointer-p cb)
      (connect-port cb (instance-get-handle instance) port-index
                    data-location))))

(defun instance-impl-deactivate (instance-ptr)
  (let ((cb (descriptor-slot-value
              (cffi:foreign-slot-value instance-ptr '(:struct instance-impl)
                                       'lv2:descriptor)
              'lv2::deactivate)))
    (unless (cffi:null-pointer-p cb)
      (cffi:foreign-funcall-pointer cb ()
        :pointer (cffi:foreign-slot-value instance-ptr '(:struct instance-impl)
                                          'lv2:handle) :void))))

(defmacro instance-?activate (instance activate-p)
  (let ((cb (gensym)))
    `(let ((,cb (descriptor-slot-value (instance-get-descriptor ,instance)
                  ',(if activate-p 'lv2::activate 'lv2::deactivate))))
       (unless (cffi:null-pointer-p ,cb)
         (setf (instance-active-p ,instance) ,activate-p)
         (cffi:foreign-funcall-pointer ,cb ()
           :pointer (instance-get-handle ,instance) :void)))))

(declaim (inline instance-activate))
(defun instance-activate (instance)
  (instance-?activate instance t))

(declaim (inline instance-deactivate))
(defun instance-deactivate (instance)
  (instance-?activate instance nil))

(declaim (inline instance-run))
(defun instance-run (instance sample-count)
  (cffi:foreign-funcall-pointer
    (descriptor-slot-value (instance-get-descriptor instance) 'lv2::run)
    () :pointer (instance-get-handle instance) :uint32 sample-count :void))
