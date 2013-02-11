(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(cl-glfw cl-opengl cl-glu))
  (setq *read-default-float-format* 'double-float))

;(declaim (optimize (debug 3) (speed 1) (safety 3)))

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

(defclass vec2 ()
  ((x :accessor x :initarg :x :type number)
   (y :accessor y :initarg :y :type number)))
(defmethod print-object ((v vec2) stream)
  (format stream "<v ~,1f ~,1f>" (x v) (y v)))
(defclass ray ()
  ((start :accessor start :initarg :start :type vec2)
   (direction :accessor direction :initarg :direction :type vec2)))

(defmethod collapse ((r ray) val)
  "Given a ray and a value, calculate the position start+val*dir along
this ray."
  (with-slots (start direction) r
    (v+ start (s* direction val))))

#+nil
(collapse (make-instance 'ray :start (v 1.0 1.0) :direction (v 1.0))
	  10.0)

(define-condition one-solution () ())
(define-condition no-solution () ())

(defun unstable-quadratic-roots (a b c)
  "This is just to verify the numerically stable version."
  (values
   (/ (* 2 c)
      (+ (- b) (sqrt (- (* b b) (* 4 a c)))))
   (/ (* 2 c)
      (- (- b) (sqrt (- (* b b) (* 4 a c)))))))

#+nil
(unstable-quadratic-roots 1.0 0.0 -4.0)
#+nil
(quadratic-roots 1.0 0.0 -4.0)


(defun quadratic-roots (a b c)
  (declare (double-float a b c)
	   (values double-float double-float &optional))
  "Find the two roots of ax^2+bx+c=0 and return them as multiple
  values."
  ;; see numerical recipes sec 5.6, p. 251 on how to avoid roundoff
  ;; error
  (let ((discriminant2 (- (* b b) (* 4 a c))))
    (unless (<= 0d0 discriminant2)
      (error 'no-solution))
    (let* ((positive-discriminant2 discriminant2)
	   (q (* -.5d0 (+ b (* (if (< b 0)
				   -1.0 1.0)
			       (sqrt positive-discriminant2)))))
	   (aa (abs a))
	   (aq (abs q)))
      (declare ((double-float 0d0) positive-discriminant2))
      (cond ((and (< aq 1d-12) (< aa 1d-12)) (error 'no-solution))
	    ((or (< aq 1d-12) (< aa 1d-12)) (error 'one-solution))
	    (t (values (/ q a) (/ c q)))))))



(defun v (&optional (x 0.0) (y 0.0))
  (make-instance 'vec2 :x x :y y))

(defmethod v+ ((u vec2) (v vec2))
  (make-instance 'vec2
		 :x (+ (x u) (x v))
		 :y (+ (y u) (y v))))
(defmethod v- ((u vec2) (v vec2))
  (make-instance 'vec2
		 :x (- (x u) (x v))
		 :y (- (y u) (y v))))
(defmethod v. ((u vec2) (v vec2))
  (+ (* (x u) (x v))
     (* (y u) (y v))))
(defmethod s* ((v vec2) s)
  (make-instance 'vec2
		 :x (* s (x v))
		 :y (* s (y v))))


(defmethod intersect ((ray ray) (circ circle))
  ;; (c-x)^2=r^2 defines the sphere, substitute x with the rays p+alpha a,
  ;; (l+alpha a)^2 = r^2
  ;; l^2 + alpha l a + alpha^2 a^2 = r^2
  ;; alpha^2 + l a alpha + l^2-r^2 = 0
  ;; alpha^2 +  b  alpha +    c    = 0 
  ;; the raydirection should have length 1, solve the quadratic equation
  (with-slots (start direction) ray
    (with-slots (radius pos-x pos-y) circ
     (let* ((center (v pos-x pos-y))
	    (l (v- center start))
	    (c (- (v. l l) (* radius radius)))
	    (b (* -2d0 (v. l direction))))
       (handler-case (multiple-value-bind (x1 x2) (quadratic-roots 1d0 b c)
		       (values
			x1 x2
			(collapse ray x1)
			(collapse ray x2)))
	 (one-solution () nil)
	 (no-solution () nil))))))

#+nil
(intersect (make-instance 'ray :direction (v 1.0)
			  :start (v 0.0 1.0))
	   (make-instance 'circle :radius 1. :pos-x 2.0 :pos-y 0.0))


(defmethod intersect ((c circle) (d circle))
  (with-slots ((r radius) (x pos-x) (y pos-y)) c
    (with-slots ((rr radius) (xx pos-x) (yy pos-y)) d
      )))
;; rayt/simple-ray has the analytical solution


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


(defmethod paint ((obj disk) (tex texture) color)
  (declare (type (unsigned-byte 8) color))
  (with-slots ((x pos-x) (y pos-y) (r radius)) obj
    (let ((r2 (* r r)))
     (with-slots (data) tex
       (destructuring-bind (h w) (array-dimensions data)
	 (loop for i from (floor (- x r)) upto (ceiling (+ x r)) do
	      (loop for j from (floor (- y r)) upto (ceiling (+ y r)) do
		   (when (and (<= 0 i (1- w))
			      (<= 0 j (1- h))
			      (< (+ (expt (- i x) 2) 
				    (expt (- j y) 2))
				 r2))
		     (setf (aref data j i) color)))))))))

(let ((rot 0)
      (parm (make-instance 'window-params
			   :pos-x 421 :pos-y 15
			   :win-w 184 :win-h 200)))
  (defun draw-frame ()
    (when (needs-update-p parm)
      (with-slots (pos-x pos-y win-w win-h 
			 needs-update-p) parm
	(glfw:set-window-pos pos-x pos-y)
	(glfw:set-window-size win-w win-h)
	(setf needs-update-p nil)))
    (setup-screen)
    (color 1 1 1)

    (let ((r 6)
	  (pos '((40 14) (80 14) (90 14)))
	  (off '((20 100))))
     (with-slots (win-w win-h) parm
       (let* ((data (make-array (list win-h win-w)
				:element-type '(unsigned-byte 8)
				:initial-element 80))
	      (tex (make-instance 'texture :data data)))
	 (dolist (p (append pos off))
	   (destructuring-bind (x y) p
	     (paint (make-instance 'disk :radius r :pos-x x :pos-y y)
		    tex 164)))
	 (let ((x0 40) (y0 60))
	   (loop for j from -30 upto 30 do
	    (setf (aref (data tex) (+ y0 j) x0)
		  255))
	   (dotimes (i 50)
	     (setf (aref (data tex) y0 (+ x0 i))
		   255))
	   (dotimes (i 50)
	     (loop for angle from -90 upto 90 do
		  (setf (aref (data tex)
			      (+ y0 (floor angle 3))
			      (+ x0 i))
			(intersect-p i angle
				     (loop for e in off collect
					  (destructuring-bind (x y) e
					    (make-instance 'circle
							   :radius r
							   :pos-x x
							   :pos-y y))))))))
	 (draw tex)
	 (dolist (p pos)
	   (destructuring-bind (x y) p
	     (color .2 1 0)
	     (draw (make-instance 'circle :radius r 
				  :pos-x (+ x .5) :pos-y (+ y .5)))))
	 (dolist (p off)
	   (destructuring-bind (x y) p
	     (color 1 .3 0)
	     (draw (make-instance 'circle :radius r 
				  :pos-x (+ x .5) :pos-y (+ y .5)))))
	 )))))


(defmethod draw ((obj circle))
  (with-slots ((x pos-x) (y pos-y) (r radius)) obj
   (with-pushed-matrix 
     (translate x y 0)
     (scale r r 1)
     (with-primitive :line-loop
       (let ((n 17))
	(dotimes (i n)
	  (vertex (sin (* 2 pi (/ i n)))
		  (cos (* 2 pi (/ i n))))))))))

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
