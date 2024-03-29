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
   (pos-z :accessor pos-z :initarg :pos-z)
   (radius :accessor radius :initarg :radius)))

(defclass disk (circle) ())

(defclass sphere ()
  ((center :accessor center :initarg :center :type vec)
   (radius :accessor radius :initarg :radius)))

(defclass vec2 ()
  ((x :accessor x :initarg :x :type number)
   (y :accessor y :initarg :y :type number)))

(defclass vec (vec2)
  ((z :accessor z :initarg :z :type number)))
(defmethod print-object ((v vec2) stream)
  (format stream "<v ~,1f ~,1f>" (x v) (y v)))
(defmethod print-object ((v vec) stream)
  (format stream "<v ~,1f ~,1f ~,1f>" (x v) (y v) (z v)))
(defclass line2 ()
  ((start :accessor start :initarg :start :type vec2)
   (target :accessor target :initarg :target :type vec2)))
(defclass line ()
  ((start :accessor start :initarg :start :type vec)
   (target :accessor target :initarg :target :type vec)))
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



(defun v (&optional (x 0.0) (y 0.0) (z 0.0))
  (make-instance 'vec :x x :y y :z z))

(defun v2 (&optional (x 0.0) (y 0.0))
  (make-instance 'vec2 :x x :y y))

(defmethod v+ ((u vec) (v vec))
  (v (+ (x u) (x v))
     (+ (y u) (y v))
     (+ (z u) (z v))))
(defmethod v- ((u vec) (v vec))
  (v (- (x u) (x v))
     (- (y u) (y v))
     (- (z u) (z v))))
(defmethod v. ((u vec) (v vec))
  (+ (* (x u) (x v))
     (* (y u) (y v))
     (* (z u) (z v))))

(defmethod v^ ((u vec) (v vec))
  "cross product"
  (let ((a (x u)) (b (y u)) (c (z u))
	(x (x v)) (y (y v)) (z (z v)))
    (v (- (* b z) (* c y))
       (- (* c x) (* a z))
       (- (* a y) (* b x)))))
#+nil
(v^ (v 1 0 0)
    (v 0 1 0))

(defmethod v+ ((u vec2) (v vec2))
  (v2 (+ (x u) (x v))
      (+ (y u) (y v))))
(defmethod v- ((u vec2) (v vec2))
  (v2 (- (x u) (x v))
      (- (y u) (y v))))
(defmethod v. ((u vec2) (v vec2))
  (+ (* (x u) (x v))
     (* (y u) (y v))))

(defmethod norm ((v vec2))
  (sqrt (v. v v)))
(defmethod norm ((v vec))
  (sqrt (v. v v)))

(defmethod normalize ((v vec2))
  (s* v (/ (norm v))))
(defmethod normalize ((v vec))
  (s* v (/ (norm v))))

(defmethod perpendicular ((v vec2))
  (let ((q (normalize v)))
    (v (- (y q)) (x q))))

(defmethod perpendicular ((vec vec))
  (let* ((x (normalize vec))
	 (q (if (< (z x) (* 2/3 (norm x)))
		(v 0.0 0.0 1.0)
		(v 0.0 1.0 0.0))))
    (v^ x q)))

#+nil
(perpendicular (v 1 1 0))

(defmethod angle ((u vec2) (v vec2))
  (let ((l (v- u v)))
    (atan (y l) (x l))))

(defmethod s* ((v vec2) s)
  (v2 (* s (x v))
      (* s (y v))))

(defmethod s* ((v vec) s)
  (v (* s (x v))
     (* s (y v))
     (* s (z v))))

(defmethod intersect ((ray ray) (circ circle))
  ;; (c-x)^2=r^2 defines the sphere, substitute x with the rays p+alpha a,
  ;; (l+alpha a)^2 = r^2
  ;; l^2 + alpha l a + alpha^2 a^2 = r^2
  ;; alpha^2 + l a alpha + l^2-r^2 = 0
  ;; alpha^2 +  b  alpha +    c    = 0 
  ;; the raydirection should have length 1, solve the quadratic equation
  ;; return the closest point first
  (with-slots (start direction) ray
    (with-slots (radius pos-x pos-y) circ
     (let* ((center (v pos-x pos-y))
	    (l (v- center start))
	    (c (- (v. l l) (* radius radius)))
	    (b (* -2d0 (v. l direction))))
       (multiple-value-bind (x1 x2) (quadratic-roots 1d0 b c)
	 (if (< x1 x2)
	     (values
	      (collapse ray x1)
	      (collapse ray x2))
	     (values
	      (collapse ray x2)
	      (collapse ray x1))))))))

(defmethod intersect ((ray ray) (sphere sphere))
  (with-slots (start direction) ray
    (with-slots (center radius) sphere
     (let* ((l (v- center start))
	    (c (- (v. l l) (* radius radius)))
	    (b (* -2d0 (v. l direction))))
       (multiple-value-bind (x1 x2) (quadratic-roots 1d0 b c)
	 (if (< x1 x2)
	     (values
	      (collapse ray x1)
	      (collapse ray x2))
	     (values
	      (collapse ray x2)
	      (collapse ray x1))))))))


(defmethod tangent-touch ((circ circle) (p vec2))
  ;; find the two points where the tangents that intersect in point p
  ;; touch the circle c
  ;; construct a second circle with radius R around p
  ;; intersect two circles, one with radius R and another with radius r
  ;; the small circle sits on the origin, the big circle is on x=R
  ;; (%i3) solve([(x-RR)^2+y^2=R^2,x^2+y^2=r^2],[x,y]);
  ;;              2                   2    2          2                 2    2
  ;;             r          r sqrt(4 R  - r )        r        r sqrt(4 R  - r )
  ;; (%o3) [[x = ---, y = - -----------------], [x = ---, y = -----------------]]
  ;;             2 R               2 R               2 R             2 R
  (with-slots ((r radius) (cx pos-x) (cy pos-y)) circ
    (with-slots (x y) p
      (let* ((c (v cx cy))
	     (l (s* (v- p c) .5)) ;; half of vector from point to
				  ;; center of circle (theorem of thales ?)
	     (rr2 (v. l l))
	     (rr (sqrt rr2))
	     (s (/ r (* 2 rr)))
	     (ix (* s r))
	     (iy (* s (sqrt (- (* 4 rr2) (* r r))))))
	(values 
	 (v+ c (v+ (s* (normalize l) ix)
		   (s* (perpendicular l) iy)))
	 (v+ c (v+ (s* (normalize l) ix)
		   (s* (perpendicular l) (- iy))))
	 (v+ c (s* (normalize l) ix)))))))

(defmethod tangent-touch ((sphere sphere) (p vec))
  (with-slots ((r radius) (c center)) sphere
    (with-slots (x y) p
      (let* ((l (s* (v- p c) .5))
	     (rr2 (v. l l))
	     (rr (sqrt rr2))
	     (s (/ r (* 2 rr)))
	     (ix (* s r))
	     (iy (* s (sqrt (- (* 4 rr2) (* r r))))))
	(values 
	 (v+ c (v+ (s* (normalize l) ix)
		   (s* (perpendicular l) iy)))
	 (v+ c (v+ (s* (normalize l) ix)
		   (s* (perpendicular l) (- iy)))))))))

#+nil
(tangent-touch (make-instance 'circle :radius 7. :pos-x 10. :pos-y 0.)
	       (v 0 0))

(defclass texture ()
  ((data :accessor data :initarg :data)
   (filter :accessor filter :initarg :filter :initform :nearest)
   (tex-format :accessor tex-format :initarg :tex-format :initform :luminance)))

(defmethod draw ((tex texture))
  (with-slots (data tex-format filter) tex
    (destructuring-bind (h w) (array-dimensions data)
	(let ((obj (first (gen-textures 1))))
	  (bind-texture :texture-2d obj)
	  ;;(pixel-store-i gl:+unpack-alignment+ 1)
	  
	  (tex-parameter :texture-2d :texture-min-filter filter)
	  (tex-parameter :texture-2d :texture-mag-filter filter)
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


#+nil
(defmethod paint ((obj line) (tex texture) color)
  (declare (type (unsigned-byte 8) color))
  (with-slots (data) tex
    (destructuring-bind (h w) (array-dimensions (d))
     (with-slots (u v) obj
       (with-slots ((ux x) (uy y)) u
	 (with-slots ((vx x) (vy y)) v
	   (let ((dx (- ux vx))
		 (dy (- uy vy)))
	     (if (< (abs dx) (abs dy))
		 (loop for j from (min vy uy) upto (max vy uy) do
		      (setf (aref data j (- j ))))))))))))

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

(defun draw-circles (r pos)
  (dolist (p pos)
    (destructuring-bind (x y) p
      (draw (make-instance 'circle :radius r 
			   :pos-x x :pos-y y)))))

(defun closest-number-divisible-by-n (n val)
  (loop for i from val below (+ val n) do
       (when (= 0 (mod i n))
	 (return-from closest-number-divisible-by-n i))))

(let ((rot 0)
      (parm (make-instance 'window-params
			   :pos-x 421 :pos-y 15
			   :win-w 184 :win-h 646)))
  (defun draw-frame ()
    (when (needs-update-p parm)
      (with-slots (pos-x pos-y win-w win-h 
			 needs-update-p) parm
	(glfw:set-window-pos pos-x pos-y)
	(glfw:set-window-size win-w win-h)
	(setf needs-update-p nil)))
    (setup-screen)
    (color 1 1 1)

    (incf rot 4)
    (when (< rot 6)
      (setf rot 6.5))
    (when (< 79 rot)
      (setf rot 6.5))
    (let ((r 9)
	  (rout 9)
	  (pos '((20 14)
		 (80 14)
		 (120 14)
		 ))
	  (off (list (list 20 (- 100 rot))
		     (list 120 40)
		     (list 80 80)
		     )))
     (with-slots (win-w win-h) parm
       (enable :blend)
       (blend-func :src-alpha :one)
       
       (with-pushed-matrix
	 (scale 3 3 1)
	 (progn
	  (color 0 1 0) (draw-circles r pos)
	  (color 1 0 0) (draw-circles rout off)
	  (point-size 2.7)
	  (line-width 2)
	  (let ((z 14)
		(h0 300))
	    (color 1 1 0) (draw-tangents z r pos rout off)
	    (draw-phase-space-2d parm pos off z r rout h0)
	    (progn
	      (color 1 1 1 1)
	      (draw-phi-coordinate-system parm h0)))))))))

(defmethod draw-phase-space-2d ((parm window-params) pos off z r rout h0)
  (loop for c in pos and target from 0 do
       (destructuring-bind (x y) c
	 (handler-case
	     (multiple-value-bind (u v)
		 (intersect (make-instance 'ray :direction (v 1.0)
					   :start (v 0.0 z))
			    (make-instance 'circle :radius r
					   :pos-x x :pos-y y))
	       (loop with n = 17 for i upto n do
		    (let ((p (v+ u (s* (v- v u) (/ i n)))))
		      (progn ;; color based on target bead number
			     ;; (spatial position)
			(color (- 1 (/ target 3)) (/ target 3) 0 .09)) 
		      (dolist (c off)
			(destructuring-bind (x y) c
			  (multiple-value-bind (lo ro)  
			      (tangent-touch
			       (make-instance 'circle :radius rout
					      :pos-x x :pos-y y)
			       p)
			    (loop with nj = 18 for j upto nj do
				 (let ((q (v+ lo (s* (v- lo ro) (/ j nj)))))
				   (handler-case
				       #+nil(draw (v (x p)
					    (+ h0 (* 180 (/ pi) (angle q p)
						     ))))
				     (draw (let ((h (+ h0 (* 180 (/ pi) (angle q p)
							     )))
						 (tx (+ 30 (* 5 target))))
					     (make-line (v tx h) (v (+ 3 tx) h))))
				     (simple-type-error ())
				     (type-error ())
				     (opengl-error ()))))))))))
	   (no-solution ())
	   (one-solution ())))))

(defmethod draw-phi-coordinate-system ((parm window-params) h0)
  (with-slots (win-w) parm
   (let ((max-angle (* 180 (/ pi) (asin (/ 1.47 1.52))))) 
     (loop for angle in (list 0 90 180 (+ 90 max-angle) (- 90 max-angle))
	do
	  (let ((h (+ h0 (*  angle))))
	    (draw (make-line (v 0 h)
			     (v win-w h))))))))

(defun draw-tangents (z r pos rout off) 
  (dolist (c pos)
    (destructuring-bind (x y) c
       (handler-case 
	   (multiple-value-bind (left right)
	       (intersect (make-instance 'ray :direction (v 1.0)
					 :start (v 0.0 z))
			  (make-instance 'circle :radius r
					 :pos-x x :pos-y y))
	     (loop with n = 1 for i upto n do
	       (let ((p (v+ left (s* (v- right left) (/ i n)))))
		(dolist (c off)
		  (destructuring-bind (x y) c
		    (handler-case 
			(multiple-value-bind (a b)
			    (tangent-touch
			     (make-instance 'circle :radius rout
					    :pos-x x :pos-y y)
			     p)
			  (dolist (q (list a b))
			    (handler-case
				(draw (make-line p q))
			      (opengl-error ()))))
		      (type-error ())
		      ))))))
	 (no-solution ())
	 (one-solution ())
	 ))))

(defmethod make-line ((u vec2) (v vec2))
  (make-instance 'line :target v :start u))


(defmethod draw ((p vec2))
  (with-primitive :points
     (vertex (x p) (y p))))

(defmethod draw ((l line))
  (with-slots ((u start) (v target)) l
    (with-primitive :lines
      (vertex (x u) (y u))
      (vertex (x v) (y v)))))

(defmethod draw ((obj circle))
  (with-slots ((x pos-x) (y pos-y) (r radius)) obj
   (with-pushed-matrix 
     (translate x y 0)
     (scale r r 1)
     (with-primitive :points
       (vertex 0 0))
     (with-primitive :line-loop
       (let ((n 80))
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
    ((glfw:swap-interval 10))
  (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
    (return-from glfw::do-open-window))
  (draw-frame))
