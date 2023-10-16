
; TM CONSTRUCTORS

(defvar *instructions* nil)

(defun instruction (q-ini g-ini q-fin g-fin mv)
  (list (list q-ini g-ini) '-> (list q-fin g-fin mv)))

(defun add-instruction (instruction)
  (push instruction *instructions*))

;;; INITIAL STATE ACCESSORS

(defun ini (instruction)
  (first instruction))

(defun instruction-ini-state (instruction)
  (first (ini instruction)))

(defun instruction-ini-char (instruction)
  (second (ini instruction)))

;;; FINAL STATE ACCESSORS

(defun fin (instruction)
  (third instruction))

(defun instruction-fin-state (instruction)
  (first (fin instruction)))

(defun instruction-fin-char (instruction)
  (second (fin instruction)))

(defun instruction-fin-move (instruction)
  (third (fin instruction)))

;;; INITIAL STATE

(defun init-state ()
  (instruction-ini-state (car (last *instructions*))))


;;; CONFIGURATION ACCESSORS

(defun string-move-right (string)
  "Moves last character of string to beginning"
  (let* ((last-char-pos (1- (length string)))
         (last-char (elt string last-char-pos)))
    (format nil "~a~a" last-char (subseq string 0 last-char-pos))))

(defun string-move-left (string)
  "Moves first character of string to end"
  (let ((first-char (elt string 0)))
    (format nil "~a~a" (subseq string 1) first-char)))

(defun conf-state-pos (conf)
  (position #\Q conf))

(defun conf-state (conf)
  (let ((pos (conf-state-pos conf)))
    (read-from-string (subseq conf
                              pos
                              (+ pos 2)))))

(defun state-on-beginning-p (conf)
  (= (conf-state-pos conf) 0))

(defun state-on-end-p (conf)
  (= (conf-state-pos conf) 
     (- (length conf) 2)))

(defun conf-next-char (conf)
  (if (state-on-end-p conf)
      'b
    (read-from-string (string (elt conf (+ (conf-state-pos conf) 2))))))

;;; MOVE STATE FUNCTIONS

(defun move-state-right (conf)
  (if (state-on-end-p conf)
      (format nil "~ab~a" (subseq conf 0 (- (length conf) 2)) (conf-state conf))
    (let* ((state-pos (conf-state-pos conf))
           (state-sub (subseq conf state-pos (+ state-pos 3))))
      (progn
        (setf 
         (subseq conf state-pos (+ state-pos 3))
         (string-move-right state-sub))
        conf))))

(defun move-state-left (conf)
  (if (state-on-beginning-p conf)
      (format nil "~ab~a" (conf-state conf) (subseq conf 2))
    (let* ((state-pos (conf-state-pos conf))
           (state-sub (subseq conf (1- state-pos) (+ state-pos 2))))
      (progn
        (setf 
         (subseq conf (1- state-pos) (+ state-pos 2))
         (string-move-left state-sub))
        conf))))

(defun conf-move-state (conf n)
  (cond ((= n 0) conf)
        ((= n 1) (move-state-right conf))
        ((= n -1) (move-state-left conf))
        (t (error "Connot move state by ~a." n))))

;;; REWRITE CHAR FUNCTIONS

(defun symbol-to-lower (s)
  (char (format nil "~(~a~)" s) 0))

(defun conf-rewrite-char (conf symbol-char)
  (if (state-on-end-p conf)
      (format nil "~a~(~a~)" conf symbol-char)
    (progn
      (setf 
       (elt conf (+ (conf-state-pos conf) 2))
       (symbol-to-lower symbol-char))
      conf)))

;;; CHANGE STATE FUNCTIONS

(defun conf-change-state (conf new-state)
  (setf (subseq conf 
                (conf-state-pos conf)
                (+ (conf-state-pos conf) 2))
        (string new-state))
  conf)


;;; OUTPUT CLEANING FUNCTIONS

(defun front-clean-blank (string)
  (if (char= (elt string 0) #\b)
      (front-clean-blank (subseq string 1))
    string))

(defun clean-blank (string)
  (reverse
   (front-clean-blank 
    (reverse
     (front-clean-blank string)))))

(defun clean-output (conf)
  (labels ((list-to-string (lst)
             (format nil "~{~a~}" lst)))
    (loop
       for char across conf
       unless (or (char= char #\Q)
                  (char= char #\F)) collect char into result
       finally (return (clean-blank (list-to-string result))))))

;;; TM FUNCTIONS

(defun tm-next-instruction (conf)
  (or
   (find (list (conf-state conf)
               (conf-next-char conf))
         *instructions* 
         :key #'ini
         :test #'equal)
   (error "Could not find instruction, check input")))

(defun tm-step (conf)
  (let ((ins (tm-next-instruction conf)))
    (conf-change-state
     (conf-move-state
      (conf-rewrite-char conf (instruction-fin-char ins))
      (instruction-fin-move ins))
     (instruction-fin-state ins))))

(defun tm-compute (input &optional (print nil))
  (let* ((state (init-state))
         (ini-input (format nil "~a~a" state input)))
    (loop
       for input = ini-input then (tm-step input)
       for i = 0 then (1+ i)
       when print do (format t "~a:~5t~a ->~%" i input)
       when (eql (conf-state input) 'QF) do (return (clean-output input))
       when (< 10000 i) do (error "Infinite computation"))))

#|
; this turing machine converts "aaa" -> "aaaa" (or w -> ww where w = a*)
; b is blank

(add-instruction (instruction 'q1 'a 'q2 'x 1))
(add-instruction (instruction 'q1 'b 'qf 'b 0))

(add-instruction (instruction 'q2 'a 'q2 'a 1))
(add-instruction (instruction 'q2 'x 'q2 'x 1))
(add-instruction (instruction 'q2 'b 'q3 'x -1))

(add-instruction (instruction 'q3 'a 'q4 'a -1))
(add-instruction (instruction 'q3 'x 'q3 'x -1))
(add-instruction (instruction 'q3 'b 'q5 'b 1))

(add-instruction (instruction 'q4 'a 'q4 'a -1))
(add-instruction (instruction 'q4 'x 'q1 'x 1))

(add-instruction (instruction 'q5 'x 'q5 'a 1))
(add-instruction (instruction 'q5 'b 'qf 'b 0))

|#