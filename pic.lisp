
(defun run-date ()
  (let ((s (with-output-to-string (stream)
	     (sb-ext:run-program "/bin/date" nil :output stream))))
    (subseq s 0 (1- (length s)))))

(defun sq (x)
  (expt x 2))

(defun nah (eta f)
  (* eta f))

(defun fern (eta f)
  (* (- 1 eta) f))

(defun quadsolve (a b c)
  (let ((det (sqrt (- (* b b) (* 4 a c)))))
    (values (/ (- det b)
	       (* 2 a))
	    (/ (- det b)
	       (* 2 a)))))
  
(with-open-file (str "/dev/shm/o.eps" :if-exists :supersede
		     :if-does-not-exist :create
		     :direction :output)
 (macrolet ((eps (&rest args)
	      `(funcall #'format str ,@args)))
   (labels ((text (x y string &optional (size 20))
	      (eps "/Helvetica findfont~%~d scalefont~%setfont~%newpath ~f ~f moveto~%(~a) show~%" 
		   size x y string))
	    (arc (x y radius &optional (angle-start 0) (angle-end 360))
	      (eps "~f ~f ~f ~f ~f arc~%"
		   x y radius angle-start angle-end))
	    (setlinewidth (&optional (width 2))
	      (eps "~f setlinewidth~%" width))
	    (setgray (&optional (gray 0))
	      (eps "~f setgray~%" gray))
	    (setrgbcolor (&optional (r 1.0) (g 1.0) (b 1.0))
	      (eps "~f ~f ~f setrgbcolor~%" r g b))
	    (translate (x y)
	      (eps "~f ~f translate~%" x y))
	    (stroke ()
	      (eps "stroke~%"))
	    (newpath ()
	      (eps "newpath~%"))
	    (closepath ()
	      (eps "closepath~%"))
	    (line (x y w h)
	      (eps "~f ~f moveto ~f ~f rlineto~%" x y w h))
	    (moveto (x y)
	      (eps "~f ~f moveto~%" x y))
	    (rlineto (x y)
	      (eps "~f ~f rlineto~%" x y))
	    (rmoveto (x y)
	      (eps "~f ~f rmoveto~%" x y))
	    (lineto (x y)
	      (eps "~f ~f lineto~%" x y))
	    (currentpoint ()
	      (eps "currentpoint~%"))
	    (save ()
	      (eps "save~%"))
	    (restore ()
	      (eps "restore~%"))
	    (dup ()
	      (eps "dup~%"))
	    (index (i)
	      (eps "~d index~%" i))
	    (dot (&optional (radius .9))
	      (currentpoint) ;; put current position on stack
	      (index 1) ;; duplicate the two coordinates for arc and moveto
	      (index 1)
	      (eps "~f 0 360 arc~%" radius)
	      (eps "moveto~%"))
	    (fills ()
	      (eps "fill~%"))
	    (clip ()
	      (eps "clip~%"))
	    (grestore ()
	      (eps "grestore~%"))
	    (initclip ()
	      (eps "initclip~%"))
	    (rectangle (x y w h)
	      (let ((w2 (/ w 2))
		    (h2 (/ h 2))) 
		(eps "newpath
~f ~f moveto
0 ~f rlineto
~f 0 rlineto
0 ~f rlineto
~f 0 rlineto
closepath
" 
		     (- x w2) (- y h2)
		     h w (- h) (- w))))
	    (finish ()
	      (eps "%%Trailer~%%%Pages: ~d~%%%EOF" 1))
	    (start ()
	      (eps 
	       "%!PS-Adobe-3.0
%%Creator: martin
%%Title: sketch
%%CreationDate: ~S
%%DocumentData: Clean7Bit
%%Origin: 0 0
%%LanguageLevel: 2 
%%Pages: (atend)
%%PageOrder: Ascend
%%BoundingBox: 0 0 800 400
%%EndComments
%%BeginProlog
%%EndProlog
%%BeginSetup
/page-begin {
    gsave
} def
/page-end {
    grestore
    showpage
} def
%%EndSetup
" (run-date))))

     (start) 
     (eps "%%Page: 1 1~%page-begin~%")
     (setgray .4)
     (text 750 12 (format nil "~d/28" 2))
     (setlinewidth .2)
     (eps "1 setlinejoin~%")
     (let* ((f1 100)
	    (eta1 .5)
	    (r1 10)
	    (kappa1 .4)
	    (alpha1 (atan (/ (* r1 kappa1)
			 f1)))
	    (f2 100)
	    (f3 100)
	    (eta3 .3)
	    (f4 90)
	    (eta4 .7)
	    (f5 20))
       (moveto 10 200)
       (let* ((fern1 (fern eta1 f1))
	      (q (/ (* fern1 (sin alpha1))
		    (sin (- (/ pi 4) alpha1))))
	      (ta (tan alpha1))
	      #+nil (w (quadsolve (1+ (sq ta))
				  (* 2 fern1 (sq ta))
				  (- (sq (* ta fern1)) (sq q))))
	      #+nil (l (sqrt (- (sq q) (sq w))))
	      (l (/ (* ta fern1)
		    (- 1 ta))))
	 (dot)
	 (rlineto (+ fern1 l) l) ;; angled beam going up / hit mirror
	 (let* ((p (- (nah eta1 f1) l))
		(rho (* p ta)))
	   (rlineto rho p) ;; hit lens L1
	   (dot)
	   (rlineto r1 f1) ;; MMA
	   (dot)
	   (rlineto r1 (- f1)) ;; L1
	   (dot)
	   (rlineto (- l) (- (+ (nah eta1 f1) l))) ;; M1
	   (rlineto (- (fern eta1 f1) l) l)	 ;; B1
	   (dot)
	   (let ((xi (* f2 ta)))		; height at L2
	     (rlineto f2 xi)		;; L2
	     (rlineto f2 0)
	     (dot)
	     (rlineto f3 0)	   ;; L3
	     (rlineto f3 (- xi))	   ;; LCOS
	     (rlineto (- f3) (- xi)) ;; L3
	     (dot)
	     (rlineto (- (+ (nah eta3 f3) xi)) 0) ;; PBS
	     (rlineto 0 (- xi (fern eta3 f3)))    ;; BFP
	     (rlineto 0 (- f5))		       ;; L5
	     (rlineto xi (- f5))		       ;; object
	     (stroke)
	     (moveto 10 200) ;; ray that goes down, and will be absorbed in B1
	     (let* ((qu (/ (* fern1 (sin alpha1))
			   (sin (+ (/ pi 4) alpha1))))
		    #+nil (wu (quadsolve (+ 1 (sq ta))
					 (* -2 fern1 (sq ta))
					 (- (* (sq ta) (sq fern1))
					    (sq qu))))
		    #+nil (lu (- (sq qu) (sq wu)))
		    (lu (/ (* ta fern1)
			   (1+ ta))))
	      
	       (rlineto (- fern1 lu) (- lu)) ;; M1
	       (let ((y (+ (nah eta1 f1) lu)))
		 (rlineto (* -1 y ta) y)) ;; L1
	       (dot)
	       (rlineto r1 f1) ;; MMA
	       (dot)
	       (let* ((beta (* (/ PI 180) 1.8))
		     ; distance between central beam and deflected beam on lens
		     (b (* f1 (- ta (tan (- alpha1 beta))))))
		 (rlineto (- r1 b) (- f1))
		 (rmoveto (* kappa1 r1) 0) ;; draw deflected central beam
		 (rlineto (- (- r1 b)) f1)))
	     )
	   ))

       ;; optic axis
       (moveto 10 200)
       (rlineto (fern eta1 f1) 0) ;; mirror
       (rlineto 0 (nah eta1 f1))	;; L1
       (dot)
       (rlineto r1 f1)
       (dot) ;; MMA
       (rlineto r1 (- f1))
       (rlineto 0 (- (nah eta1 f1)))
       (rlineto (fern eta1 f1) 0)
       (dot) ;; B1
       (rlineto f2 0)
       (dot) ;; L2
       (rlineto f2 0)
       (dot) 
       (rlineto f3 0)
       (dot) ;; L3
       (rlineto f3 0)
       (dot) ;; LCOS
       (rmoveto (- (+ f3 (nah eta3 f3))) 0)
       (dot) ;; PBS
       (rlineto 0 (- (fern eta3 f3)))
       (dot) ;; BFP
       (rlineto 0 (- f5))
       (dot) ;; L5 objective
       (rlineto 0 (- f5))
       (dot) ;; object
       (rmoveto 0 (+ (* 2 f5)
		     (fern eta4 f4)))
       (dot) ;; D1 dichroic
       (rlineto (nah eta4 f4) 0)
       (dot) ;; L4
       (rlineto f4 0)
       (dot) ;; camera
       (stroke))
     (eps "page-end~%")
     (finish))))
