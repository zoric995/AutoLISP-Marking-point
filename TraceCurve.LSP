(defun IsOnThisLine (pt objPLine)
  (zerop
    (distance
      (trans (vlax-curve-getClosestPointTo objPLine (trans pt 1 0))
	     0
	     1
      )
      pt
    )
  )
)
(defun c:TraceCurve
	       (/      olderr oldcmd oldosmode	   e	  obj	 stpt
		enpt   from   stop   D	    refpt  TD	  pt	 cd
		d1     cd2
	       )
  (setq	oldcmd	  (getvar "cmdecho")
	oldosmode (getvar "osmode")
	olderr	  *error*
  )
  (defun *error* (msg)
    (if	(not
	  (member msg (list "Function cancelled" "quit / exit abort"))
	)
      (princ (strcat "MARK Error: " msg))
    )
    ;(setvar "cmdecho" oldcmd)
    ;(setvar "osmode" oldosmode)
    (setq *error* olderr)
    (princ)
  )
  (setvar "cmdecho" 0)
  ;(setvar "osmode" 0)
  (vl-load-com)
  (while (not stop)
    (if
      (setq e (entsel "\nSelect a LINE near the end to measure from: "))
       (progn
	 (setq pt (osnap (cadr e) "Near"))
	 (if (cdr (assoc 0 (entget (setq e (car e)))))
	   (setq stop T)
	   (princ "\nNot a LINE!!! Try again!!!")
	 )
       )
       (princ "\nNo object found!!! Try again!!!")
    )
  )
  (setq	obj  (vlax-ename->vla-object e)
	stpt (trans (vlax-curve-getStartPoint obj) 0 1)
	enpt (trans (vlax-curve-getEndPoint obj) 0 1)
	stop nil
  )
  (if (< (distance pt stpt) (distance pt enpt))
    (setq from	"StartPoint"
	  refpt	stpt
    )
    (setq from	"EndPoint"
	  refpt	enpt
    )
  )
(princ "Total Length: ")
    (if (eq (vla-get-objectname obj) "AcDbArc")
    (princ (setq TD (vlax-get-property obj 'ArcLength)))
    (princ (setq TD (vlax-get-property obj 'Length)))
)
;;; Mark First base point
  (if (/= 34 (getvar "pdmode"))
    (setvar "pdmode" 34)
  )
  (command "_.point" refpt)
  (command "_.chprop" (entlast) "" "Color" "blue" "")
  (setvar "osmode" oldosmode)
  (princ
    (strcat "\nSpecify base point (or Press ENTER to Accept "
	    from
	    "):"
    )
  )
  (initget 32)
  (setq tmp (getpoint refpt))
  (if tmp
    (if	(IsOnThisLine tmp obj)
;;; Check on this Line?
      (progn
	(setq refpt tmp)
	(command "_.point" refpt)
	(command "_.chprop" (entlast) "" "Color" "red" "")
	(cond
	  ((= "StartPoint" from)
	   (setq d1 (vlax-curve-getDistAtPoint obj (trans refpt 1 0)))
	  )
	  ((= "EndPoint" from)
	   (setq d1
		  (- TD (vlax-curve-getDistAtPoint obj (trans refpt 1 0)))
	   )
	  )
	)
      )
      (progn (princ "\nThe selected point is not on the Line!!!")
	     (exit)
      )
    )
    (progn
      (setq d1 0.0)
    )
  )
  (cond
    ((= "StartPoint" from)
     (setq cd d1)			; Cumulative distance
    )
    ((= "EndPoint" from)
     (setq cd (- TD d1))
    )
  )
  (while (not stop)
    (setvar "osmode" oldosmode)
    (initget 161)
    (setq D
	   (getdist refpt
		    "\nSpecify distance (or press ENTER to exit): "
	   )
    )
    (if	(= 'REAL (type D))
      (progn
	(cond
	  ((= "StartPoint" from)
	   (setq cd (+ cd D))
	   (princ "\nCumulative distance = ")
	   (princ cd)
	  )
	  ((= "EndPoint" from)
	   (setq cd (- cd D))
	   (princ "\nCumulative distance = ")
	   (princ (- TD cd))
	  )
	)
	(princ " Total Length = ")
	(princ TD)
	(if (setq pt (vlax-curve-getPointAtDist obj cd))
	  (progn
	    ;(setvar "osmode" 0)
	    (setq refpt (trans pt 0 1))
	    (command "_.point" refpt)
	    (if	(or (equal cd 0.0 0.001) (equal cd TD 0.001))
	      (setq stop T)
	    )
	  )				;progn
	  (progn
	    (princ
	      "\nCumulative Input distance is more than Total length!!!"
	    )
	    (cond
	      ((= "StartPoint" from)
	       (setq cd (- cd D))
	      )
	      ((= "EndPoint" from)
	       (setq cd (+ cd D))
	      )
	    )
	  )
	)				;if
      )					;progn
      (setq stop T)
    )
  )					;while
  (setvar "cmdecho" oldcmd)
  (setvar "osmode" oldosmode)
  (setq *error* olderr)
  (princ)
)
;;;

