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

(define-condition simple-audio-player-error (error)
  ((player :accessor sap-error-player
           :initarg :player)
   (code :accessor sap-error-code
         :initarg :code
         :type (signed-byte 32)))
  (:default-initargs :player nil :code +e-fail+)
  (:report report-sap-error))

(define-condition simple-audio-player-released
    (simple-audio-player-error)
  ()
  (:report "Simple audio player is released."))

(defun report-sap-error (sap-error stream)
  (declare (type simple-audio-player-error sap-error)
           (type stream stream))
  (let ((code (logand #xFFFFFFFF (the (signed-byte 32)
                                      (sap-error-code sap-error)))))
    (cffi:with-foreign-object (pb :pointer)
      (let ((s (if (plusp (format-message
                           +format-message-flags+
                           (cffi:null-pointer)
                           code
                           0
                           pb
                           0
                           (cffi:null-pointer)))
                 (prog1 (cffi:foreign-string-to-lisp 
                         (cffi:mem-ref pb :pointer)
                         :encoding :utf-16/le)
                   (local-free (cffi:mem-ref pb :pointer)))
                 (format nil "Simple Audio Player error #x~8,'0x"
                         code))))
        (write-line (string-trim '(#\tab #\space #\return #\newline) s)
                    stream))))
  sap-error)

(defclass simple-audio-player ()
  ((pointer :initform (make-safe-pointer)
            :accessor sap-pointer
            :type safe-pointer)
   (callback :initarg :callback
             :accessor sap-callback
             :type (or null function)
             :documentation
"Callback function(of two args) which is called on player state change.")
   (source :initform nil
           :reader sap-source
           :writer set-sap-source
           :type (or null simple-string)
           :documentation "Source media path."))
  (:default-initargs :callback nil)
  (:documentation
"
Audio player class.
 Constructor accepts :CALLBACK initarg, which
  must be either NIL or function of two arguments
    (the player and new player state),
  which is called whenever media playback state changes.
"))

(defun sap-release (player)
"Releases all resources occupied by PLAYER."
  (declare (type simple-audio-player player))
  (when (slot-boundp player 'pointer)
    (release-pointer (sap-pointer player)))
  (values))

(defun sap-released-p (player)
"Returns T when player resources are initialized."
  (declare (type simple-audio-player player))
  (or (not (slot-boundp player 'pointer))
      (not (sp-alive-p (sap-pointer player)))))

(defun sap-check-alive (player)
  (declare (type simple-audio-player player))
  (when (sap-released-p player)
    (error 'simple-audio-player-released :player player))
  t)

(declaim (inline sap-check-hr))
(defun sap-check-hr (player hr)
  (declare (type (or null simple-audio-player) player)
           (type (signed-byte 32) hr))
  (when (minusp hr)
    (error 'simple-audio-player-error :player player :code hr))
  t)

(defmethod shared-initialize
    ((player simple-audio-player) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (with-accessors ((pointer sap-pointer)) player
    (sap-release player)
    (call-next-method)
    (cffi:with-foreign-object (pp :pointer)
      (let ((pointer (sap-pointer player))
            (finalizer (lambda () (release-pointer pointer))))
        (sap-check-hr nil (sap-create-player pp))
        (setf (sp-pointer pointer) (cffi:mem-ref pp :pointer)
              (sp-alive-p pointer) t)
        (tg:finalize player finalizer)
        (sap-check-hr nil (sap-invoke
                           set-callback pointer
                           :pointer (cffi:callback sap-callable)
                           :pointer (cffi:null-pointer)
                           :int))
        (setf (ptr-object (cffi:mem-ref pp :pointer)) player)
        player))))

(defmacro with-simple-audio-player ((var value) &body body)
"
VALUE, which must be an SIMPLE-AUDIO-PLAYER is bound to
VAR for the duration of BODY forms execution. On escape from
dynamic context of execution, player is released by the means of
SAP-RELEASE.
"
  `(let ((,var ,value))
     (unwind-protect
          (progn ,@body)
       (when (typep ,var 'simple-audio-player)
         (sap-release ,var)))))

(defun sap-open (player filename)
"Opens media file and initializes player."
  (declare (type simple-audio-player player)
           (type (or pathname string) filename))
  (setf filename (string filename))
  (sap-check-alive player)
  (set-sap-source filename player)
  (sap-check-hr
   player
   (sap-invoke open (sap-pointer player)
               (:string :encoding :utf-16/le) filename
               :int)))

(defun sap-play (player)
"Starts media stream playback."
  (declare (type simple-audio-player player))
  (sap-check-alive player)
  (sap-check-hr player (sap-invoke play (sap-pointer player) :int)))

(defun sap-pause (player)
"Pauses media stream playback."
  (declare (type simple-audio-player player))
  (sap-check-alive player)
  (sap-check-hr player (sap-invoke pause (sap-pointer player) :int)))

(defun sap-stop (player)
"Stops media stream playback."
  (declare (type simple-audio-player player))
  (sap-check-alive player)
  (sap-check-hr player (sap-invoke stop (sap-pointer player) :int)))

(defun sap-wait (player)
"Waits until audio stream is completed."
  (declare (type simple-audio-player player))
  (sap-check-alive player)
  (sap-check-hr player (sap-invoke wait (sap-pointer player) :int)))

(defun sap-state (player)
"Gets player state. One of :STOPPED, :PAUSED, :PLAYING"
  (declare (type simple-audio-player player))
  (sap-check-alive player)
  (cffi:with-foreign-object (p :int)
    (sap-check-hr player (sap-invoke get-state (sap-pointer player)
                                     :pointer p
                                     :int))
    (case (cffi:mem-ref p :int)
      (1 :paused)
      (2 :playing)
      (t :stopped))))

(defun sap-volume (player)
"Gets audio volume, in percent."
  (declare (type simple-audio-player player))
  (sap-check-alive player)
  (cffi:with-foreign-object (p :int)
    (sap-check-hr player (sap-invoke get-volume (sap-pointer player)
                                     :pointer p
                                     :int))
    (the fixnum (cffi:mem-ref p :int))))

(defun sap-duration (player)
"Gets duration, in seconds, of the media stream."
  (declare (type simple-audio-player player))
  (sap-check-alive player)
  (cffi:with-foreign-object (p :int)
    (sap-check-hr player (sap-invoke get-duration (sap-pointer player)
                                     :pointer p
                                     :int))
    (the fixnum (cffi:mem-ref p :int))))

(defun sap-position (player)
"Gets position, in seconds, inside media stream."
  (declare (type simple-audio-player player))
  (sap-check-alive player)
  (cffi:with-foreign-object (p :int)
    (sap-check-hr player (sap-invoke get-position (sap-pointer player)
                                     :pointer p
                                     :int))
    (the fixnum (cffi:mem-ref p :int))))

(defun (setf sap-state) (new-state player)
"Sets new player state. One of :STOPPED, :PAUSED, :PLAYING."
  (declare (type simple-audio-player player)
           (type (member :stopped :paused :playing) new-state))
  (ecase new-state
    (:stopped (sap-stop player))
    (:paused (sap-pause player))
    (:playing (sap-play player)))
  new-state)

(defun (setf sap-volume) (new-volume player)
"Sets new audio volume(in percent)."
  (declare (type simple-audio-player player)
           (type fixnum new-volume))
  (sap-check-alive player)
  (sap-check-hr player (sap-invoke set-volume (sap-pointer player)
                                   :int new-volume
                                   :int))
  new-volume)

(defun (setf sap-position) (new-position player)
 "Sets current position(in seconds) inside media stream."
  (declare (type simple-audio-player player)
           (type fixnum new-position))
  (sap-check-alive player)
  (sap-check-hr player (sap-invoke set-position (sap-pointer player)
                                   :int new-position
                                   :int))
  new-position)

(defun (setf sap-source) (new-source player)
  (declare (type simple-audio-player player)
           (type (or null pathname string) new-source))
  (sap-check-alive player)
  (unless (equal (sap-source player) new-source)
    (if (null new-source)
      (progn (sap-stop player)
             (set-sap-source nil player))
      (sap-open player new-source)))
  new-source)

(cffi:defcallback (sap-callable :convention :stdcall)
    :void ((ptr :pointer) (state :int) (data :pointer))
  (declare (ignore data))
  (let ((sap (ptr-object ptr)))
    (unless (null sap)
      (let ((callback (sap-callback sap)))
        (unless (null callback)
          (funcall callback sap (case state
                                  (1 :paused)
                                  (2 :playing)
                                  (t :stopped))))))))

(defun sap-init-com (&optional apartment)
"
Initializes COM library on current thread.
In case when APARTMENT is non-NIL, initializes COM STA
  environment. Otherwise initializes MTA.
Returns T when apartment has been successfully initialized.
"
  (let ((hr (co-initialize-ex
             (cffi:null-pointer)
             (if apartment
               +coinit-apartment+ 
               +coinit-multithreaded+))))
    (>= hr 0)))

(defun sap-close-com ()
"
Decrements COM library reference count on current thread.
"
  (co-uninitialize)
  (values))

;;;; vim: ft=lisp et
