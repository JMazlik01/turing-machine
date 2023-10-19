# turing-machine

Simple Turing machine in CL.


## How to use

Create new turing machine by calling `turing-machine` function

```Lisp
; args are instructions, initial-state, final-state (list) and optional description
> (turing-machine nil 'q1 '(q2 q3) "Example turing machine")

(:INSTRUCTIONS NIL 
 :INITIAL-STATE Q1 
 :FINAL-STATE (Q2 Q3) 
 :DESCRIPTION "Example turing machine")

> (description *)

"Example turing machine"
```

You can add new instructions with `add-instruction` function and `instruction` constructor

```Lisp
; Adds instruction (Q1, b) -> (Q2, 0, -1) into turing-machine
> (add-instruction (instruction 'q1 1 'q2 0 -1) tm)
```
With complete instruction set you can now start Turing machine on input using `tm-compute`:
```Lisp

> (tm-compute "a" tm)

"aa"

; if optional last arg is t, function prints configuration in every step
> (tm-compute "a" tm t)

0:   Q1a ->
1:   xQ2 ->
2:   Q3xx ->
3:   Q3bxx ->
4:   bQ5xx ->
5:   baQ5x ->
6:   baaQ5 ->
7:   baaQFb ->
"aa"
```

You can print all machines instructions with `print-tm-instructions`:
```Lisp
> (print-tm-instructions tm2)

(Q2, b) -> (QF, b, 0)
(Q2, 0) -> (Q2, 1, 1)
(Q2, 1) -> (Q2, 0, 1)
T
```

## Examples

```Lisp
; this turing machine converts "aaa" -> "aaaa" (or w -> ww where w = a*)
; b is blank

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
```

```Lisp
; this turing machine flips all bits

(prog1 
  (setf tm2 (turing-machine nil 'q2 '(qf) "Flips all bits"))
  (add-instruction (instruction 'q2 '1 'q2 '0 1) tm2)
  (add-instruction (instruction 'q2 '0 'q2 '1 1) tm2)
  (add-instruction (instruction 'q2 'b 'qf 'b 0) tm2))

(tm-compute "1010111" tm2 t)

; You can also chain calls
(tm-compute (tm-compute "1010111" tm2) tm2) ; returns input
```

## Notes

- Symbol b is reserved for blank character
- Every symbol from alphabet is just one character long (can't have a1 character)