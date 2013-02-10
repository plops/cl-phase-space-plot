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

(defclass circle ()
  ((pos-x :accessor pos-x :initarg :pos-x)
   (pos-y :accessor pos-y :initarg :pos-y)
   (radius :accessor radius :initarg :radius)))

(defclass disk (circle) ())

(defclass texture ()
  ((data :accessor data :initarg :data)
   (tex-format :accessor tex-format :initarg :tex-format :initform :luminance)))

(defmethod draw ((tex texture))
  (with-slots (data tex-format) tex
    (destructuring-bind (h w) (array-dimensions data)
	(let ((obj (first (gen-textures 1))))
	  (bind-texture :texture-2d obj)
	  ;;(pixel-store-i gl:+unpack-alignment+ 1)
	  (tex-parameter :texture-2d :texture-min-filter :nearest)
	  (tex-parameter :texture-2d :texture-mag-filter :nearest)
	  (progn
	    (bind-texture :texture-2d obj)
	    (tex-image-2d :texture-2d 0 :rgba w h 0
			  tex-format :unsigned-byte 
			  (sb-sys:vector-sap
			   (sb-ext:array-storage-vector data))))
	  (enable :texture-2d)
	  (with-primitive :quads 
	    (labels ((c (a b)
		       (tex-coord (/ a w) (/ b h))
		       (vertex a b)))
	      (c 0 0)          (c 0 h)
	      (c w h) (c w 0)))
	  (disable :texture-2d)
	  (delete-textures (list obj))))))

(defun setup-screen ()
  (destructuring-bind (w h) (glfw:get-window-size)
    (viewport 0 0 w h)
    (matrix-mode :projection)
    (load-identity)
    (glu:ortho-2d 0 w 0 h)
    (matrix-mode :modelview)
    (clear-color 0 0 0 1)
    (clear :color-buffer-bit)))

(let ((rot 0)
      (parm (make-instance 'window-params
			   :pos-x 421 :pos-y 15
			   :win-w 186 :win-h 200)))
  (defun draw-frame ()
    (when (needs-update-p parm)
      (with-slots (pos-x pos-y win-w win-h 
			 needs-update-p) parm
	(glfw:set-window-pos pos-x pos-y)
	(glfw:set-window-size win-w win-h)
	(setf needs-update-p nil)))
    (setup-screen)
    (color 1 1 1)

    (draw (make-instance 'texture
			 :data (make-array (list 64 64)
					   :element-type '(unsigned-byte 8)
					   :initial-element 120)))
    (draw (make-instance 'circle :radius 14 :pos-x 60 :pos-y 20))))



(defmethod draw ((obj circle))
  (with-slots ((x pos-x) (y pos-y) (r radius)) obj
   (with-pushed-matrix 
     (translate x y 0)
     (scale r r 1)
     (with-primitive :line-loop
       (dotimes (i 17)
	 (vertex (sin (* 2 pi (/ i 17)))
		 (cos (* 2 pi (/ i 17)))))))))

(defmethod draw ((obj disk))
  (with-slots ((x pos-x) (y pos-y) (r radius)) obj
   (with-pushed-matrix 
     (translate x y 0)
     (scale r r 1)
     (with-primitive :triangle-fan
       (vertex 0 0)
       (dotimes (i 17)
	 (vertex (sin (* 2 pi (/ i 17)))
		 (cos (* 2 pi (/ i 17)))))
       (vertex 0 1)))))

#+nil
(glfw:do-window (:title "display grid on projector" 
			:width 120 :height 120)
    ((glfw:swap-interval 1))
  (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
    (return-from glfw::do-open-window))
  (draw-frame))
