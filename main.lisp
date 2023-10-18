
(defvar *current-tm* nil)

;;; TM INSTRUCTIONS

(defun instruction (q-ini g-ini q-fin g-fin mv)
  (list (list q-ini g-ini) '-> (list q-fin g-fin mv)))

;; initial state accessors

(defun ini (instruction)
  (first instruction))

(defun instruction-ini-state (instruction)
  (first (ini instruction)))

(defun instruction-ini-char (instruction)
  (second (ini instruction)))

;; final state accessors

(defun fin (instruction)
  (third instruction))

(defun instruction-fin-state (instruction)
  (first (fin instruction)))

(defun instruction-fin-char (instruction)
  (second (fin instruction)))

(defun instruction-fin-move (instruction)
  (third (fin instruction)))

(defun print-instruction (instruction)
  (format t "(~a, ~(~a~)) -> (~a, ~(~a~), ~a)~%"
          (instruction-ini-state instruction)
          (instruction-ini-char instruction)
          (instruction-fin-state instruction)
          (instruction-fin-char instruction)
          (instruction-fin-move instruction)))

;;; CONFIGURATION FUNCTIONS

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
  "Returns index position of state in configuration"
  (position #\Q conf))

(defun conf-state (conf)
  "Returns symbol of current state"
  (let ((pos (conf-state-pos conf)))
    (read-from-string (subseq conf pos (+ pos 2)))))

(defun state-on-beginning-p (conf)
  (= (conf-state-pos conf) 0))

(defun state-on-end-p (conf)
  (= (conf-state-pos conf) 
     (- (length conf) 2)))

(defun conf-next-char (conf)
  "Returns current character read by head"
  (if (state-on-end-p conf)
      'b
    (read-from-string (string (elt conf (+ (conf-state-pos conf) 2))))))


;;; TURING MACHINE

(defun turing-machine (instructions initial-state final-state &optional (description nil))
  (list :instructions instructions
        :initial-state initial-state
        :final-state final-state
        :description description))

(defun instructions (tm)
  (getf tm :instructions))

(defun (setf instructions) (value tm)
  (setf (getf tm :instructions) value))

(defun add-instruction (instruction turing-machine)
  (push instruction (instructions turing-machine)))

(defun initial-state (tm)
  (getf tm :initial-state))

(defun final-state (tm)
  (getf tm :final-state))

(defun description (tm)
  (getf tm :description))

(defun tm-end-p (tm conf)
  (find (conf-state conf) (final-state tm)))

(defun print-tm-instructions (tm)
  (mapcar #'print-instruction (instructions tm))
  t)


;;; MOVE STATE FUNCTIONS

(defun move-state-right (conf)
  "Moves state right in config"
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
  "Moves state left in config"
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
  "Moves state by n"
  (cond ((= n 0) conf)
        ((= n 1) (move-state-right conf))
        ((= n -1) (move-state-left conf))
        (t (error "Connot move state by ~a." n))))


;;; REWRITE CHAR FUNCTIONS

(defun symbol-to-lower (s)
  "Returns symbol s as lowercase character"
  (char (format nil "~(~a~)" s) 0))

(defun conf-rewrite-char (conf symbol-char)
  "Rewrites next character"
  (if (state-on-end-p conf)
      (format nil "~a~(~a~)" conf symbol-char)
    (progn
      (setf 
       (elt conf (+ (conf-state-pos conf) 2))
       (symbol-to-lower symbol-char))
      conf)))


;;; CHANGE STATE FUNCTIONS

(defun conf-change-state (conf new-state)
  "Changes state in configuration"
  (setf (subseq conf 
                (conf-state-pos conf)
                (+ (conf-state-pos conf) 2))
        (string new-state))
  conf)


;;; OUTPUT CLEANING FUNCTIONS

(defun front-clean-blank (string)
  "Removes blank characters from start of configuration"
  (if (and (not (string-equal string "")) 
           (char= (elt string 0) #\b))
      (front-clean-blank (subseq string 1))
    string))

(defun clean-blank (string)
  "Removes blank characters fron configuration"
  (reverse
   (front-clean-blank 
    (reverse
     (front-clean-blank string)))))

(defun clean-output (conf)
  "Removes state and blank symbols"
  (labels ((list-to-string (lst)
             (format nil "~{~a~}" lst)))
    (loop
       for char across conf
       unless (or (char= char #\Q)
                  (char= char #\F)) collect char into result
       finally (return (clean-blank (list-to-string result))))))


;;; TM FUNCTIONS

(defun tm-next-instruction (tm conf)
  "Returns next instruction to apply"
  (or
   (find (list (conf-state conf)
               (conf-next-char conf))
         (instructions tm)
         :key #'ini
         :test #'equal)
   (error "Could not find instruction, check input")))

(defun tm-step (tm conf)
  (let ((ins (tm-next-instruction tm conf)))
    (conf-change-state
     (conf-move-state
      (conf-rewrite-char conf (instruction-fin-char ins))
      (instruction-fin-move ins))
     (instruction-fin-state ins))))

(defun tm-compute (input tm &optional (print nil))
  "Computes output for given input"
  (let* ((state (initial-state tm))
         (ini-input (format nil "~a~a" state input)))
    (loop
       for input = ini-input then (tm-step tm input)
       for i = 0 then (1+ i)
       when print do (format t "~a:~5t~a ->~%" i input)
       when (tm-end-p tm input) do (return (clean-output input))
       when (< 10000 i) do (error "Infinite computation"))))

#|
EXAMPLES

(prog1 
  (setf tm (turing-machine nil 'q1 '(qf) "For word w returns ww"))
  (add-instruction (instruction 'q1 'a 'q2 'x 1) tm)
  (add-instruction (instruction 'q1 'b 'qf 'b 0) tm)

  (add-instruction (instruction 'q2 'a 'q2 'a 1) tm)
  (add-instruction (instruction 'q2 'x 'q2 'x 1) tm)
  (add-instruction (instruction 'q2 'b 'q3 'x -1) tm)

  (add-instruction (instruction 'q3 'a 'q4 'a -1) tm)
  (add-instruction (instruction 'q3 'x 'q3 'x -1) tm)
  (add-instruction (instruction 'q3 'b 'q5 'b 1) tm)

  (add-instruction (instruction 'q4 'a 'q4 'a -1) tm)
  (add-instruction (instruction 'q4 'x 'q1 'x 1) tm)

  (add-instruction (instruction 'q5 'x 'q5 'a 1) tm)
  (add-instruction (instruction 'q5 'b 'qf 'b 0) tm))

(tm-compute "aaa" tm t)



(prog1 
  (setf tm2 (turing-machine nil 'q2 '(qf) "Flips all bits"))
  (add-instruction (instruction 'q2 '1 'q2 '0 1) tm2)
  (add-instruction (instruction 'q2 '0 'q2 '1 1) tm2)
  (add-instruction (instruction 'q2 'b 'qf 'b 0) tm2))

(tm-compute "1010111" tm2 t)

You can also chain calls:

(tm-compute (tm-compute "1010111" tm2) tm2)

|#
