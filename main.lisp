(ql:quickload :lispbuilder-sdl)
(ql:quickload :bordeaux-threads)

(defpackage :scanview
  (:use #:cl)
  (:export :run))

(in-package :scanview)

(proclaim '(optimize (speed 3) (space 0) (debug 0)))

(defparameter *width* 640)
(defparameter *height* 480)

(defparameter *pixel-width* 10)
(defparameter *pixel-height* 10)

(defparameter *data-set-width* 80)
(defparameter *data-set-height* 60)

(defparameter *num-samples* (* *data-set-height* *data-set-width*))
(defparameter *max-sample-value* 255)

(defparameter *data* (make-array *num-samples* :initial-element *max-sample-value*))

(defun data-sample-to-color (sample)
  (let ((normalized (* (float (/ sample *max-sample-value*)) 255)))
    (sdl:color :r normalized :g normalized :b normalized)))

(defun draw-big-pixel (x y color)
  (sdl:draw-box-* x y *pixel-width* *pixel-height*
                :color color))

(defun draw-data-point (x y)
  (draw-big-pixel (* x *pixel-width*) (* y *pixel-height*) (data-sample-to-color (elt *data* (+ (* y *data-set-height*) x)))))

(defun draw-world ()
  (setf *data* (map 'vector (lambda (x) (random 255)) *data*))

  (dotimes (x *data-set-width*)
    (dotimes (y *data-set-height*)
      (draw-data-point x y))))

(defun main-window ()
  (sdl:WITH-INIT ()
    (sdl:WINDOW *width* *height*
                :title-caption "Scan View" :icon-caption "ScanView")
    (setf (sdl:frame-rate) 0)

    (sdl:WITH-EVENTS ()
      (:QUIT-EVENT () T)
      (:KEY-DOWN-EVENT
       (:KEY key)
       (WHEN (sdl:KEY= key :SDL-KEY-ESCAPE)
         (sdl:PUSH-QUIT-EVENT)))
      (:IDLE
       (draw-world)
       (sdl:UPDATE-DISPLAY))))
  (format t "Terminating."))

(defun run ()
;;  (bordeaux-threads:make-thread #'main-window :name "main-window") ;
  (main-window)
  "Hello world")
