;gnu clisp  2.49.60

(defun init ()
    (defvar c0) (setq c0 0)
    (defvar c1) (setq c1 0)
    (defvar c2) (setq c2 0)
    (defvar c3) (setq c3 0)
    (defvar c4) (setq c4 0)
    (defvar c5) (setq c5 0)
    (defvar c6) (setq c6 0)
    (defvar pos)
    (defvar color)
    (setq color "R")
    
    (defvar board)
    (defvar ii)
    (setq ii 0)
    (setf board (make-array '(42)))
    (loop
        (setf (aref board ii) "O")
        (setq ii (+ ii 1))
        (when (> ii 41) (return ii))
    )
)


(defun printBoard ()
    ;(setf (aref board 2) "R")
   (setq ii 41)
    (format t "--------------~%")
    (loop
        (format t "~d " (aref board ii))
        (setq ii (- ii 1))
        (if (= (mod ii 7) 6) (terpri))
        (when (< ii 0) (return ii))
      )
    (format t "--------------~%6 5 4 3 2 1 0~%")
  (terpri)(terpri)
)

(defun drop (pos color)
    (defvar offset)
    
    (if (> pos 6) (setq pos 0));Check if in bounds
    (if (< pos 0) (setq pos 0));Check if in bounds
    
    (cond ((= pos 0) (setq offset (+ pos (* c0 7))));Switch statement to determine offset multiplier
          ((= pos 1) (setq offset (+ pos (* c1 7))))
          ((= pos 2) (setq offset (+ pos (* c2 7))))
          ((= pos 3) (setq offset (+ pos (* c3 7))))
          ((= pos 4) (setq offset (+ pos (* c4 7))))
          ((= pos 5) (setq offset (+ pos (* c5 7))))
          ((= pos 6) (setq offset (+ pos (* c6 7))))
    )
    (cond ((= pos 0) (setq c0 (+ c0 1)));Switch state to update column multiplier
          ((= pos 1) (setq c1 (+ c1 1)))
          ((= pos 2) (setq c2 (+ c2 1)))
          ((= pos 3) (setq c3 (+ c3 1)))
          ((= pos 4) (setq c4 (+ c4 1)))
          ((= pos 5) (setq c5 (+ c5 1)))
          ((= pos 6) (setq c6 (+ c6 1)))
    )
    (setf (aref board offset) color);Place down the players chip
)

(defun CONFOUR ()
  (init)
  (printBoard)
  (let ((done 0) (end 0)  (toDrop 0) (jj 0) (cArr (vector c0 c1 c2 c3 c4 c5 c6)))
    (loop
      (setq cArr (vector c0 c1 c2 c3 c4 c5 c6))
      (setq jj 0)
      (setq end 1)
      (loop
        (if (< (aref cArr jj) 6) (setq end 0))
        (setq jj (+ jj 1))
        (when (= jj 7) (return jj)))
      (if (= end 1) (print "------ITS A TIE-----"))
      (if (= end 1) (return-from CONFOUR 1))
      (format t "~d's Turn: " color)
      (setf toDrop (read))
      (if (and (and (> toDrop -1) (< toDrop 7)) (= (aref cArr toDrop) 6)) (setf toDrop 10))  
      (format t "~%")
      (if (and (> toDrop -1) (< toDrop 7)) (drop toDrop color))
      (if (= toDrop -1) (return-from CONFOUR -1))
      (printboard)
      (if (and (and (> toDrop -1) (< toDrop 7)) (= (checkWinner todrop) 1))  (setq done 1))
      (if (= done 1) (format t "-------~d  W O N-------" color))
      (if (and (> toDrop -1) (< toDrop 7))
      (cond
      ((string= color "R") (setq color "Y"))
      ((string= color "Y") (setq color "R"))))       
      (when (= done 1)  (return done)))
    )
  )


(defun rightC (pos)
  
  (if (and (string= (aref board pos) color) (string= (aref board (- pos 1)) color) (string= (aref board (- pos 2)) color) (string= (aref board (- pos 3)) color)) (return-from rightC 1)) 
  (if (if (= (mod pos 7) 5)(and (string= (aref board pos) color) (string= (aref board (+ pos 1)) color) (string= (aref board (- pos 2)) color) (string= (aref board (- pos 1)) color)) )(return-from rightC 1)) 
  (if (if (= (mod pos 7) 4)(and (string= (aref board pos) color) (string= (aref board (+ pos 1)) color) (string= (aref board (+ pos 2)) color) (string= (aref board (- pos 1)) color)) )(return-from rightC 1)) 
  (return-from rightC 0)
  )
(defun leftC (pos)
  
  (if (and (string= (aref board pos) color) (string= (aref board (+ pos 1)) color) (string= (aref board (+ pos 2)) color) (string= (aref board (+ pos 3)) color)) (return-from leftC 1))   
  (if (if (= (mod pos 7) 2)(and (string= (aref board pos) color) (string= (aref board (+ pos 1)) color) (string= (aref board (- pos 2)) color) (string= (aref board (- pos 1)) color)) )(return-from leftC 1)) 
  (if (if (= (mod pos 7) 1)(and (string= (aref board pos) color) (string= (aref board (+ pos 1)) color) (string= (aref board (+ pos 2)) color) (string= (aref board (- pos 1)) color)) )(return-from leftC 1)) 
  (return-from leftC 0)
  )

(defun slantRC (pos)
  (if (and (string= (aref board pos) color) (string= (aref board (- pos 8)) color) (string= (aref board (- pos 16)) color) (string= (aref board (- pos 24)) color)) (return-from slantRC 1)) 
  (return-from slantRC 0)
  )
(defun slantLC (pos)
  (if (and (string= (aref board pos) color) (string= (aref board (- pos 6)) color) (string= (aref board (- pos 12)) color) (string= (aref board (- pos 18)) color)) (return-from slantLC 1)) 
  (return-from slantLC 0)
  )

(defun downC (pos)
  (if (and (string= (aref board pos) color) (string= (aref board (- pos 7)) color) (string= (aref board (- pos 14)) color) (string= (aref board (- pos 21)) color)) (return-from downC 1)) 
  (return-from downC 0)
  )

(defun checkWinner (pos)
  (defvar winstate)
  (defvar offset)
  (setq offset 0)
  (setq winstate 1)
  
  (cond ((= pos 0) (setq offset (+ pos (* (- c0 1) 7))));Switch statement to determine offset multiplier
        ((= pos 1) (setq offset (+ pos (* (- c1 1) 7))))
        ((= pos 2) (setq offset (+ pos (* (- c2 1) 7))))
        ((= pos 3) (setq offset (+ pos (* (- c3 1) 7))))
        ((= pos 4) (setq offset (+ pos (* (- c4 1) 7))))
        ((= pos 5) (setq offset (+ pos (* (- c5 1) 7))))
        ((= pos 6) (setq offset (+ pos (* (- c6 1) 7))))
        )
  
  (if (>= pos 3) (if (= (rightC  offset ) 1) (return-from checkWinner winstate)))
  (if (<= pos 3) (if (= (leftC offset) 1) (return-from checkWinner winstate)))
  (if (>= (/ offset 7) 3) (if (= (downC offset) 1) (return-from checkWinner winstate)))
  
  (if (>= (- c6 1) 3) (if (= (slantRC (+ 6 (* (- c6 1) 7))) 1) (return-from checkWinner winstate)))
  (if (>= (- c5 1) 3) (if (= (slantRC (+ 5 (* (- c5 1) 7))) 1) (return-from checkWinner winstate)))
  
  (if (>= (- c4 1) 3) (if (= (slantRC (+ 4 (* (- c4 1) 7))) 1) (return-from checkWinner winstate)))
  
  (if (>= (- c3 1) 3) (if (= (slantRC (+ 3 (* (- c3 1) 7))) 1) (return-from checkWinner winstate)))
  (if (>= (- c3 1) 3) (if (= (slantLC (+ 3 (* (- c3 1) 7))) 1) (return-from checkWinner winstate)))

  (if (>= (- c2 1) 3) (if (= (slantLC (+ 2 (* (- c2 1) 7))) 1) (return-from checkWinner winstate)))
  (if (>= (- c1 1) 3) (if (= (slantLC (+ 1 (* (- c1 1) 7))) 1) (return-from checkWinner winstate)))
  (if (>= (- c0 1) 3) (if (= (slantLC (* (- c0 1) 7)) 1) (return-from checkWinner winstate)))
  
  (setq winstate 0)
  (return-from checkWinner winstate)
  )

