;;; Copyright (c) 2019 Tito Latini
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

(in-package :lv2)

(defvar *uris* (make-array 250 :adjustable t :fill-pointer 0))
(defvar *urids* (make-hash-table :test 'equal))

(defun add-uri (uri)
  (setf (gethash uri *urids*) (vector-push-extend uri *uris*)))

(defun uri-to-id (uri)
  "Get the numeric identifier of a Uniform Resource Identifier,
a string that identifies a resource."
  (or (gethash uri *urids*) (add-uri uri)))

(defun id-to-uri (id)
  "Get the Uniform Resource Identifier for a mapped numeric ID
obtained from LV2:URI-TO-ID.

Example:

    (lv2:uri-to-id \"http://lv2plug.in/ns/ext/midi#MidiEvent\")
    ;; => 62
    (lv2:id-to-uri 62)
    ;; => \"http://lv2plug.in/ns/ext/midi#MidiEvent\""
  (if (< id (length *uris*)) (aref *uris* id) ""))

(cffi:defcallback urid-map :uint32 ((handle :pointer) (uri :string))
  (declare (ignore handle))
  (uri-to-id uri))

(cffi:defcallback urid-unmap :string ((handle :pointer) (id :uint32))
  (declare (ignore handle))
  (id-to-uri id))

(defun make-urid-map (&key inverse-p)
  (let ((ptr (cffi:foreign-alloc '(:struct urid-map))))
    (handler-case
        (cffi:with-foreign-slots ((handle map) ptr (:struct urid-map))
          (setf handle ptr)
          (setf map (cffi:get-callback (if inverse-p 'urid-unmap 'urid-map)))
          ptr)
      (condition (c)
        (cffi:foreign-free ptr)
        (error c)))))

;;; Fill the tables.
(let ((uris
       '(("http://lv2plug.in/ns/ext/atom"
          "Atom" "AtomPort" "Blank" "Bool" "Chunk" "Double" "Event" "Float"
          "Int" "Literal" "Long" "Number" "Object" "Path" "Property"
          "Resource" "Sequence" "Sound" "String" "Tuple" "URI" "URID"
          "Vector" "atomTransfer" "beatTime" "bufferType" "childType"
          "eventTransfer" "frameTime" "supports" "timeUnit")
         ("http://lv2plug.in/ns/ext/buf-size"
          "boundedBlockLength" "fixedBlockLength" "maxBlockLength"
          "minBlockLength" "nominalBlockLength" "powerOf2BlockLength"
          "sequenceSize")
         ("http://lv2plug.in/ns/ext/event"
          "Event" "EventPort" "FrameStamp" "TimeStamp" "generatesTimeStamp"
          "generic" "inheritsEvent" "inheritsTimeStamp" "supportsEvent"
          "supportsTimeStamp")
         ("http://lv2plug.in/ns/ext/log"
          "Entry" "Error" "Note" "Trace" "Warning" "log")
         ("http://lv2plug.in/ns/ext/midi"
          "ActiveSense" "Aftertouch" "Bender" "ChannelPressure" "Chunk"
          "Clock" "Continue" "Controller" "MidiEvent" "NoteOff" "NoteOn"
          "ProgramChange" "QuarterFrame" "Reset" "SongPosition"
          "SongSelect" "Start" "Stop" "SystemCommon" "SystemExclusive"
          "SystemMessage" "SystemRealtime" "Tick" "TuneRequest"
          "VoiceMessage" "benderValue" "binding" "byteNumber" "channel"
          "chunk" "controllerNumber" "controllerValue" "noteNumber"
          "pressure" "programNumber" "property" "songNumber"
          "songPosition" "status" "statusMask" "velocity")
         ("http://lv2plug.in/ns/ext/morph"
          "AutoMorphPort" "MorphPort" "interface" "supportsType"
          "currentType")
         ("http://lv2plug.in/ns/ext/options"
          "Option" "interface" "options" "requiredOption" "supportedOption")
         ("http://lv2plug.in/ns/ext/parameters"
          "CompressorControls" "ControlGroup" "EnvelopeControls"
          "FilterControls" "OscillatorControls" "amplitude" "attack"
          "bypass" "cutoffFrequency" "decay" "delay" "dryLevel"
          "frequency" "gain" "hold" "pulseWidth" "ratio" "release"
          "resonance" "sampleRate" "sustain" "threshold" "waveform"
          "wetDryRatio" "wetLevel")
         ("http://lv2plug.in/ns/ext/patch"
          "Ack" "Delete" "Copy" "Error" "Get" "Message" "Move" "Patch"
          "Post" "Put" "Request" "Response" "Set" "add" "body"
          "destination" "property" "readable" "remove" "request"
          "subject" "sequenceNumber" "value" "wildcard" "writable")
         ("http://lv2plug.in/ns/ext/port-groups"
          "DiscreteGroup" "Element" "FivePointOneGroup" "FivePointZeroGroup"
          "FourPointZeroGroup" "Group" "InputGroup" "MidSideGroup"
          "MonoGroup" "OutputGroup" "SevenPointOneGroup"
          "SevenPointOneWideGroup" "SixPointOneGroup" "StereoGroup"
          "ThreePointZeroGroup" "center" "centerLeft" "centerRight"
          "element" "group" "left" "lowFrequencyEffects" "mainInput"
          "mainOutput" "rearCenter" "rearLeft" "rearRight" "right"
          "side" "sideChainOf" "sideLeft" "sideRight" "source" "subGroupOf")
         ("http://lv2plug.in/ns/ext/port-props"
          "causesArtifacts" "continuousCV" "discreteCV" "displayPriority"
          "expensive" "hasStrictBounds" "logarithmic" "notAutomatic"
          "notOnGUI" "rangeSteps" "supportsStrictBounds" "trigger")
         ("http://lv2plug.in/ns/ext/presets"
          "Bank" "Preset" "bank" "preset" "value")
         ("http://lv2plug.in/ns/ext/resize-port"
          "asLargeAs" "minimumSize" "resize")
         ("http://lv2plug.in/ns/ext/state"
          "State" "interface" "loadDefaultState" "makePath"
          "mapPath" "state" "threadSafeRestore")
         ("http://lv2plug.in/ns/ext/time"
          "Time" "Position" "Rate" "position" "barBeat" "bar"
          "beat" "beatUnit" "beatsPerBar" "beatsPerMinute"
          "frame" "framesPerSecond" "speed")
         ("http://lv2plug.in/ns/ext/urid"
          "map" "unmap")
         ("http://lv2plug.in/ns/ext/worker"
          "interface" "schedule"))))
  (loop for (prefix . rest) in uris do
       (loop for item in rest do
             (add-uri (concatenate 'string prefix "#" item)))))
