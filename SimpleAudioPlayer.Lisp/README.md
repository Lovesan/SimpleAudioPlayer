Lisp interface for SimpleAudioPlayer.dll

Make sure that dll is in your PATH before loading the library.

```lisp
(defparameter *url* "http://some-domain.com/music.mp3")

(defparameter *player*
  (make-instance 'cl-sap:simple-audio-player
                 :callback (lambda (sap state)
                             (declare (ignore sap))
                             (format t "New state: ~:(~a~)~%" state))))

(setf (cl-sap:sap-source *player*) *url*
      (cl-sap:sap-state *player*) :playing)
```

See documentation for exported symbols in :cl-sap package
  for more information.

Oh, and by the way... If you don't use some COM interop libraries, you should
  initialize COM explicitly before creating an instance of player.
  Call `(cl-sap:sap-init-com)`