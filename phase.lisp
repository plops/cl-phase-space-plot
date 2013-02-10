(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(cl-glfw cl-opengl cl-glu)))

(declaim (optimize (debug 3) (speed 1) (safety 3)))

(defpackage :disp
  (:use :cl :gl))

(in-package :disp)

(defclass window-params ()
  ((pos-x :accessor pos-x :initarg :pos-x)
   (pos-y :accessor pos-y :initarg :pos-y)
   (win-w :accessor win-w :initarg :win-w)
   (win-h :accessor win-h :initarg :win-h)
   (needs-update-p :accessor needs-update-p :initform t)))

(let ((rot 0)
      (parm (make-instance 'window-params
			   :pos-x 421 :pos-y 15
			   :win-w 186 :win-h 200)))
  (defun draw ()
    (when (needs-update-p parm)
      (with-slots (pos-x pos-y win-w win-h 
			 needs-update-p) parm
	(glfw:set-window-pos pos-x pos-y)
	(glfw:set-window-size win-w win-h)
	(setf needs-update-p nil)))
    (destructuring-bind (w h) (glfw:get-window-size)
      (progn
	(viewport 0 0 w h)
	(matrix-mode :projection)
	(load-identity)
	(glu:ortho-2d 0 w 0 h)
	(matrix-mode :modelview)
	(clear-color 0 0 0 1)
	(clear :color-buffer-bit))
      (color 1 1 1)
      (disk 12 30 20))))
(defun circle (r x y)
  (with-pushed-matrix 
    (translate x y 0)
    (scale r r 1)
    (with-primitive :line-loop
      (dotimes (i 17)
	(vertex (sin (* 2 pi (/ i 17)))
		(cos (* 2 pi (/ i 17))))))))

(defun disk (r x y)
  (with-pushed-matrix 
    (translate x y 0)
    (scale r r 1)
    (with-primitive :triangle-fan
      (vertex 0 0)
      (dotimes (i 17)
	(vertex (sin (* 2 pi (/ i 17)))
		(cos (* 2 pi (/ i 17)))))
      (vertex 0 1))))

#+nil
(glfw:do-window (:title "display grid on projector" 
			:width 120 :height 120)
    ((glfw:swap-interval 1))
  (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
    (return-from glfw::do-open-window))
  (draw))
