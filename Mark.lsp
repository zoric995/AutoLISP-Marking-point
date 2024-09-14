(defun c:Mark
	      (/ olderr	oldcmd oldosmode e obj stpt enpt from stop D
	       refpt TD	pt)
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
    (setvar "cmdecho" oldcmd)
    (setvar "osmode" oldosmode)
    (setq *error* olderr)
    (princ)
  )
  (setvar "cmdecho" 0)
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




  (while (not stop)
    (setvar "osmode" oldosmode)
    (initget 160)
    (setq D
	   (getdist refpt
		    "\nSpecify distance (or press ENTER to exit): "
	   )
    )
    (if	(= 'REAL (type D))
      (progn
	(if (<= D TD)
	  (progn
	    (cond
	      ((= "StartPoint" from)
	       (setq pt (trans (vlax-curve-getPointAtDist obj D) 0 1))
	      )
	      ((= "EndPoint" from)
	       (setq
		 pt
		  (trans (vlax-curve-getPointAtDist obj (- TD D)) 0 1)
	       )
	      )
	    )
	    (if	(/= 34 (getvar "pdmode"))
	      (setvar "pdmode" 34)
	    )
	    (setvar "osmode" 0)
	    (command "_.point" pt)
	    (princ "Mark a point from ")
	    (princ from)
	    (princ " To ")
	    (princ D)
	    (princ ".")
	  )
	  (princ "\nInput distance is more than Total length!!!")
	)
      )
      (setq stop T)
    )
  )					;while
  (setvar "cmdecho" oldcmd)
  (setvar "osmode" oldosmode)
  (setq *error* olderr)
  (princ)
)