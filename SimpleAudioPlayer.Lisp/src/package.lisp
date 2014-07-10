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

(in-package #:cl-user)

(defpackage #:cl-sap
  (:use #:cl)
  (:export
   ;; classes
   #:simple-audio-player
   #:simple-audio-player-error
   #:simple-audio-player-wrong-state
   #:simple-audio-player-released
   
   ;; methods
   #:sap-error-code
   #:sap-error-player

   #:sap-callback
   #:sap-release
   #:sap-released-p
   #:sap-open
   #:sap-play
   #:sap-pause
   #:sap-stop
   #:sap-wait
   #:sap-state
   #:sap-duration
   #:sap-volume
   #:sap-position
   #:sap-source
   #:sap-autoplay

   ;; com initialization utils
   #:sap-init-com
   #:sap-close-com

   ;; macros
   #:with-simple-audio-player
   ))

;;;; vim: ft=lisp et
