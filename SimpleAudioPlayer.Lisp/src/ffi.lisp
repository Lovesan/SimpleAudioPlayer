;;;; -*- mode:lisp;indent-tabs-mode:nil -*-

;;; Copyright (C) 2014, Dmitry Ignatiev <lovesan.ru at gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:cl-sap)

(defconstant +e-fail+ (- #x80004005 #xFFFFFFFF 1))
(defconstant +vfw-e-wrong-state+ (- #x80040227 #xFFFFFFFF 1))

(defconstant +coinit-multithreaded+ 0)
(defconstant +coinit-apartment+ 2)

(defconstant +format-message-allocate-buffer+ #x100)
(defconstant +format-message-from-system+ #x1000)
(defconstant +format-message-ignore-inserts+ #x200)
(defconstant +format-message-flags+
  (logior +format-message-from-system+
          +format-message-ignore-inserts+
          +format-message-allocate-buffer+))

(cffi:define-foreign-library kernel32-dll
  (:windows "kernel32"))

(cffi:define-foreign-library ole32-dll
  (:windows "ole32"))

(cffi:define-foreign-library sap-dll
  (:windows "SimpleAudioPlayer"))

(cffi:use-foreign-library kernel32-dll)

(cffi:use-foreign-library ole32-dll)

(cffi:use-foreign-library sap-dll)

(cffi:defcstruct i-sap-player-vtbl
  (query-interface :pointer)
  (add-ref :pointer)
  (release :pointer)
  (open :pointer)
  (play :pointer)
  (pause :pointer)
  (stop :pointer)
  (wait :pointer)
  (set-callback :pointer)
  (get-state :pointer)
  (get-volume :pointer)
  (set-volume :pointer)
  (get-duration :pointer)
  (get-position :pointer)
  (set-position :pointer))

(declaim (inline co-initialize-ex))
(cffi:defcfun ("CoInitializeEx"
               co-initialize-ex
               :convention :stdcall
               :library ole32-dll)
    :int
  (reserved :pointer)
  (coinit :uint))

(declaim (inline co-uninitialize))
(cffi:defcfun ("CoUninitialize"
               co-uninitialize
               :convention :stdcall
               :library ole32-dll)
    :void)

(cffi:defcfun ("SAPCreatePlayer"
               sap-create-player
               :convention :stdcall
               :library sap-dll)
    :int
  (out :pointer))

(declaim (inline local-free))
(cffi:defcfun ("LocalFree"
               local-free
               :convention :stdcall
               :library kernel32-dll)
    :pointer
  (mem :pointer))

(declaim (inline format-message))
(cffi:defcfun ("FormatMessageW"
               format-message
               :convention :stdcall
               :library kernel32-dll)
    :uint
  (flags :uint)
  (src :pointer)
  (code :uint)
  (lang :uint)
  (buffer :pointer)
  (size :uint)
  (args :pointer))

(defstruct (safe-pointer (:conc-name sp-))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (alive-p nil :type boolean))

(defmacro sap-invoke (method safe-pointer &rest args)
  (declare (type symbol method))
  (let ((ptr (gensym (string :pointer)))
        (vtbl (gensym (string :vtbl)))
        (offset (cffi:foreign-slot-offset
                 '(:struct i-sap-player-vtbl)
                 method))
        (mptr (gensym (string :mptr))))
    `(let* ((,ptr (sp-pointer ,safe-pointer))
            (,vtbl (cffi:mem-ref ,ptr :pointer))
            (,mptr (cffi:mem-ref ,vtbl :pointer ,offset)))
       (declare (type cffi:foreign-pointer ,ptr ,vtbl))
       (cffi:foreign-funcall-pointer
        ,mptr (:convention :stdcall) :pointer ,ptr ,@args))))

(defun release-pointer (safe-pointer)
  (declare (type safe-pointer safe-pointer))
  (with-accessors ((alive-p sp-alive-p)
                   (pointer sp-pointer))
      safe-pointer
    (when alive-p
      (sap-invoke release safe-pointer :int)
      (setf alive-p nil
            pointer (cffi:null-pointer)))
    (values)))

(defvar *ptr-object-table* (tg:make-weak-hash-table :weakness :value))

(declaim (inline ptr-object (setf ptr-object)))

(defun ptr-object (ptr)
  (declare (type cffi:foreign-pointer ptr))
  (values (gethash (cffi:pointer-address ptr)
                   *ptr-object-table*)))

(defun (setf ptr-object) (object ptr)
  (declare (type cffi:foreign-pointer ptr))
  (values (setf (gethash (cffi:pointer-address ptr)
                         *ptr-object-table*)
                object)))

;;;; vim: ft=lisp et
