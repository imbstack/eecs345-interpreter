; Interpreter Project, Part 3
; Tom Dooner (ted27)
; Brian Stack (bis12)

(load "functionParser.scm")
(load "environment.scm")

(define return #f)
(define the-begin-environment
  (add-to-environment 'true #t
    (add-to-environment 'false #f
      (add-to-environment 'return 'None '((() ()))
))))

; The heart of it all. Creates a default environment that is used, and at the
; end of interpreting the list of things, returns whatever value is put in the
; "return" variable in the environment
(define interpret
  (lambda (filename)
    (display (true-or-falsify
      (call-function 'main '() (interpret-global-statement-list (parser filename) the-begin-environment))))
))
; (define interpret (lambda (file) (display (parser file)))) ; DEBUG ONLY

; do we need to separate this from the other path that our interpreter follows? -tom
(define interpret-global-statement-list
  (lambda (parsetree env)
    (cond
      ((null? parsetree) env)
      (else
        (interpret-global-statement-list (cdr parsetree) (interpret-global-stmt (car parsetree) env))))))

(define interpret-global-stmt
  (lambda (stmt env)
    (cond
      ((eq? (car stmt) 'function) (interpret-declare-function stmt env))
      ((eq? (car stmt) 'var) (interpret-declare stmt env))
      (else (interpret-stmt (car stmt) env))
)))

; stmt is something like '(function other (x) ((return (+ x 1))))
(define interpret-declare-function
  (lambda (stmt env)
    ; the function closure:
    (add-to-environment (cadr stmt) (cddr stmt) env)))

(define call-function
  (lambda (function values-to-bind env)
    (get-environment 'return
      (call/cc (lambda (ret) (set! return ret)
                 ;(begin (display function) (display "\n") (display (cadr (get-environment function env))) (display "\n") (display env) (display "\n")
        (interpret-statement-list (cadr (get-environment function env))
                                  (create-function-env (car (get-environment function env)) values-to-bind env)))))));)

; This function takes:
;   - a list of formal parameters from a function declaration, like:
;     '(x & y)
;   - a list of the values to bind from the calling environment, like:
;     '((+ z 3) h)
;   - the environment, the last layer of which is the global environment,
; and prepares a new environment that consists of the global environment with a
; new layer which contains only the call-by-value parameters bound.
(define create-function-env
  (lambda (formal-parameters values-to-bind env)
    (bind-formal-parameters
      formal-parameters
      values-to-bind
      env
      (add-layer (global-env-only env))))) ;(this could just be in call-function ?)

(define bind-formal-parameters
  (lambda (formal-parameter-list value-list env new-env)
    (cond
      ((null? formal-parameter-list) new-env)
      (else (if (eq? (car formal-parameter-list) '&)
              ; if calling by reference, store the original variable's box in the new environment
              (bind-formal-parameters (cddr formal-parameter-list) (cdr value-list) env new-env)
              ; if calling by value, store the original variable's value in the new environment
              (bind-formal-parameters (cdr formal-parameter-list) (cdr value-list) env (add-to-environment (car formal-parameter-list) (interpret-stmt-value (car value-list) env) new-env)))))))

; Interpret all of the statements (hopefully)
(define interpret-statement-list
  (lambda (parsetree env)
    (cond
      ((null? parsetree) env)
      (else
        ; This passes the updated environment into the next statement to
        ; be interpreted
        (interpret-statement-list (cdr parsetree) (interpret-stmt (car parsetree) env))))))

; Returns the new environment after the stmt is interpreted.
(define interpret-stmt
  (lambda (stmt env)
    (cond
      ((number? stmt) env)
      ((atom? stmt) env)

      ; unary operations like (- (= x 3)) and (! (== (= x 3) y))
      ((or (eq? (car stmt) '!)
           (and (eq? (car stmt) '-) (null? (cddr stmt))))
       (interpret-stmt (cadr stmt) env))

      ; binary operation like (+ (= x 2) 3)
      ((or (member? (car stmt) '(+ - * / % -))
           (boolean-stmt? stmt))
       (interpret-stmt (caddr stmt) (interpret-stmt (cadr stmt) env)))

      ((eq? (car stmt) 'begin) (interpret-block stmt env))
      ((eq? (car stmt) 'while) (call/cc (lambda (break) (interpret-while stmt (add-to-environment 'break break (add-layer env))))))
      ((eq? (car stmt) 'break) ((get-environment 'break env) (del-layer (del-layer env))))
      ((eq? (car stmt) 'continue) ((get-environment 'continue env) env))
      ((eq? (car stmt) 'var) (interpret-declare stmt env))
      ((eq? (car stmt) '=) (interpret-assign stmt env))
      ((eq? (car stmt) 'if) (interpret-branch stmt env))
      ((eq? (car stmt) 'return) (interpret-ret stmt env))
      ((eq? (car stmt) 'funcall) (call-function (cadr stmt) (cddr stmt) env))
)))

; Performs the actions in a block
(define interpret-block
  (lambda (stmt env)
    (del-layer
      (call/cc
        (lambda (cont)
          (interpret-statement-list (cdr stmt) (add-to-environment 'continue cont (add-layer env))))))))

; Returns the value of the stmt based on the environment that is passed in.
(define interpret-stmt-value
  (lambda (stmt env)
    (cond
      ((null? stmt) 'None)
      ((number? stmt) stmt)
      ((atom? stmt) (get-environment stmt env))
      ((eq? (car stmt) 'funcall) (call-function (cadr stmt) (cddr stmt) env))
      ((eq? (car stmt) '=) (interpret-assign-value stmt env))
      ((eq? (car stmt) '+) ((interpret-binary +) stmt env))
      ((eq? (car stmt) '-) (interpret-negative stmt env))
      ((eq? (car stmt) '*) ((interpret-binary *) stmt env))
      ((eq? (car stmt) '/) ((interpret-binary (lambda (x y) (floor (/ x y)))) stmt env))
      ((eq? (car stmt) '%) ((interpret-binary remainder) stmt env))
      ((boolean-stmt? stmt) (interpret-bool-value stmt env))
)))

; Handles '(return x)
; Returns updated environment
(define interpret-ret
  (lambda (stmt env)
    (return (update-environment 'return (interpret-stmt-value (cadr stmt) env) env))))

; Handles '(if (...) (...) (...))
; Returns updated environment
(define interpret-branch
  (lambda (stmt env)
    (if (interpret-bool-value (cadr stmt) env)
      ; if true
      (interpret-stmt (caddr stmt) env)
      ; if false
      (if (null? (cdddr stmt))
        env   ; <- if there is no "else"
        (interpret-stmt (cadddr stmt) env)))))

; Handles while loops
; Returns updated environment
(define interpret-while
  (lambda (stmt env)
    (if (interpret-bool-value (cadr stmt) env)
      (interpret-while stmt (interpret-stmt (caddr stmt) env))
      env)))

; Handles '(> (= x (+ x 1)) y)
; Returns updated environment
(define interpret-bool-env
  (lambda (stmt env)
    (cond
      ((eq? stmt 'true) env)
      ((eq? stmt 'false) env)
      ((atom? stmt) env)
      (else
        (if (null? (cddr stmt))
          ; unary boolean operator, i.e. (! x):
          (interpret-stmt (cadr stmt) env)
          ; binary boolean operator:
          (interpret-stmt (cadr stmt) env))))))

; Handles '(> (= x (+ x 1)) y)
; Returns #t or #f based on the environment and the variables
(define interpret-bool-value
  (lambda (stmt env)
    (cond
      ((atom? stmt) (get-environment stmt env))
      ((eq? (car stmt) '==) ((interpret-binary eq?) stmt env))
      ((eq? (car stmt) '!=) ((interpret-binary (lambda (x y) (not (eq? x y)))) stmt env))
      ((eq? (car stmt) '<) ((interpret-binary <) stmt env))
      ((eq? (car stmt) '>) ((interpret-binary >) stmt env))
      ((eq? (car stmt) '<=) ((interpret-binary <=) stmt env))
      ((eq? (car stmt) '>=) ((interpret-binary >=) stmt env))
      ((eq? (car stmt) '&&) ((interpret-binary (lambda (x y) (and x y))) stmt env))
      ((eq? (car stmt) '||) ((interpret-binary (lambda (x y) (or x y))) stmt env))
      ((eq? (car stmt) '!) (interpret-not stmt env))
)))

; Handles '(var x)
; Returns updated environment
(define interpret-declare
  (lambda (stmt env)
    (cond
      ((null? (cddr stmt)) (add-to-environment (cadr stmt) 'None env))
      (else (add-to-environment (cadr stmt) (interpret-stmt-value (caddr stmt) env) env)))))

; Handles '(= x (+ x 1))
; Returns updated environment
(define interpret-assign
  (lambda (stmt env)
    (update-environment (cadr stmt) (interpret-stmt-value (caddr stmt) env) env)))

; Handles '(= x (+ x 1))
; Returns value, which is just the value of the caddr of it.
(define interpret-assign-value
  (lambda (stmt env)
    (cond
      ((number? (caddr stmt)) (get-environment (cadr stmt) (update-environment (cadr stmt) (caddr stmt) env)))
      (else (get-environment (cadr stmt) (update-environment (cadr stmt) (interpret-stmt-value (caddr stmt) env) env))))))

; Helper function to abstract out binary operations like < > + - && ||, etc.
(define interpret-binary
  (lambda (op)
    (lambda (stmt env)
      (op
        (interpret-stmt-value (cadr stmt) env)
        (interpret-stmt-value (caddr stmt) env)))))

; Helper function for unary or binary subtraction
(define interpret-negative
  (lambda (stmt env)
    (cond
      ((eq? (length stmt) 3) ((interpret-binary -) stmt env))
      (else (- 0 (interpret-stmt-value (cadr stmt) env))))))

; Helper function for unary !
(define interpret-not
  (lambda (stmt env)
    (not (interpret-stmt-value (cadr stmt) env))))

; Helper function to convert #t to true and #f to false
(define true-or-falsify
  (lambda (b)
    (cond
      ((eq? #t b) 'true)
      ((eq? #f b) 'false)
      (else b))))

; Test if x is an atom or not
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

; Test if the statement is boolean
(define boolean-stmt?
  (lambda (stmt)
    (or (eq? (car stmt) '==)
        (eq? (car stmt) '!=)
        (eq? (car stmt) '<)
        (eq? (car stmt) '>)
        (eq? (car stmt) '<=)
        (eq? (car stmt) '>=)
        (eq? (car stmt) '&&)
        (eq? (car stmt) '||)
        (eq? (car stmt) '!))))
