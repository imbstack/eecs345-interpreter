; Interpreter Project, Part 1
; Tom Dooner (ted27)
; Brian Stack (bis12)

(load "functionParser.scm")

(define return #f)
(define the-begin-environment
 '(((true false return) (#t #f None))))

; The heart of it all. Creates a default environment that is used, and at the
; end of interpreting the list of things, returns whatever value is put in the
; "return" variable in the environment
(define interpret
  (lambda (filename)
    (display (true-or-falsify
      (call-function 'main (interpret-global-statement-list (parser filename) the-begin-environment))))
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
  (lambda (function env)
    (get-environment 'return
      (call/cc (lambda (ret) (set! return ret)
        ; TODO!!!!! ----------------v  This will only work for functions with no parameters.
        (interpret-statement-list (cadr (get-environment function env)) env))))))

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
)))

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
    (if (interpret-bool-value (cadr stmt) (interpret-bool-env (cadr stmt) env))
      ; if true
      (interpret-stmt (caddr stmt) (interpret-bool-env (cadr stmt) env))
      ; if false
      (if (null? (cdddr stmt))
        (interpret-bool-env (cadr stmt) env)   ; <- if there is no "else"
        (interpret-stmt (cadddr stmt) (interpret-bool-env (cadr stmt) env))))))

; Handles while loops
; Returns updated environment
(define interpret-while
  (lambda (stmt env)
    (if (interpret-bool-value (cadr stmt) (interpret-bool-env (cadr stmt) env))
      (interpret-while stmt (interpret-stmt (caddr stmt) (interpret-bool-env (cadr stmt) env)))
      (interpret-stmt (cadr stmt) (interpret-bool-env (cadr stmt) env)))))

; Handles '(> (= x (+ x 1)) y)
; Returns updated environment
(define interpret-bool-env
  (lambda (stmt env)
    (cond
      ((eq? stmt 'true) env)
      ((eq? stmt 'false) env)
      (else
        (if (null? (cddr stmt))
          ; unary boolean operator, i.e. (! x):
          (interpret-stmt (cadr stmt) env)
          ; binary boolean operator:
          (interpret-stmt (cadr stmt) (interpret-stmt (caddr stmt) env)))))))

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
      (else (add-to-environment (cadr stmt)
              (interpret-stmt-value (caddr stmt) env)
              (interpret-stmt (caddr stmt) env))))))

; Handles '(= x (+ x 1))
; Returns updated environment
(define interpret-assign
  (lambda (stmt env)
    (update-environment (cadr stmt) (interpret-stmt-value (caddr stmt) env) (interpret-stmt (caddr stmt) env))))

; Handles '(= x (+ x 1))
; Returns value, which is just the value of the caddr of it.
(define interpret-assign-value
  (lambda (stmt env)
    (cond
      ((number? (caddr stmt)) (caddr stmt))
      (else (interpret-stmt-value (caddr stmt) env)))))

; Helper function to abstract out binary operations like < > + - && ||, etc.
(define interpret-binary
  (lambda (op)
    (lambda (stmt env)
      (op
        (interpret-stmt-value (cadr stmt) env)
        (interpret-stmt-value (caddr stmt) (interpret-stmt (cadr stmt) env))))))

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

; Helper function to add to the environment with the value 'None and return the
; new environment
(define add-to-environment
  (lambda (binding value env)
      (cons
        (add-to-layer binding value (car env))
        (cdr env))
))

(define add-to-layer
  (lambda (binding value layer)
    (if (declared? binding layer)
      (error "Error: You have already declared this variable in this scope!")
      (cons
        (cons binding (car layer))
        (cons (cons value (car (cdr layer))) '())))))

; Helper function to updates the value of a variable in the environment and
; return the new environment
(define update-environment
  (lambda (binding value env)
    (cond
      ((null? env) (error "Error: Trying to assign to an undeclared variable!"))
      ((member? binding (car (car env)))
       (cons (set-layer binding value (car env)) (cdr env)))
      (else
        (cons (car env) (update-environment binding value (cdr env))))
      )))

(define add-layer
  (lambda (env)
    (cons '(()()) env)))

(define del-layer
  (lambda (env)
    (cdr env)))

(define set-layer
  (lambda (binding value layer)
    (cond
      ((null? layer) layer)
      ((eq? (car (car layer)) binding)
       (cons (car layer) (cons (cons value (cdr (cadr layer))) '())))
      (else
        (let
          ((next-env (set-layer binding value (cons (cdr (car layer)) (cons (cdr (car (cdr layer))) '())))))
          ; note: here we're using add-to-layer as a utility to do the cons'ing
          (add-to-layer (car (car layer)) (car (car (cdr layer))) next-env))
))))

; Helper function to retrieve the value of a variable from the environment
(define get-environment
  (lambda (binding env)
    (cond
      ((null? env) (error "Error: This variable has not been declared yet!"))
      ((member? binding (car (car env))) (get-from-layer binding (car env)))
      (else (get-environment binding (cdr env))))))

; Given a layer in the environment like '((x y z) (1 2 3)) it will look up the
; value of a binding
(define get-from-layer
  (lambda (binding layer)
    (cond
      ((null? layer) layer)
      ((eq? (car (car layer)) binding) (car (car (cdr layer))))
      (else (get-from-layer binding
              (cons (cdr (car layer)) (cons (cdr (car (cdr layer))) '())))))))

; Helper function to convert #t to true and #f to false
(define true-or-falsify
  (lambda (b)
    (cond
      ((eq? #t b) 'true)
      ((eq? #f b) 'false)
      (else b))))

; Helper function to return #t if binding is in some level of the environment
; and #f if not.
(define declared?
  (lambda (binding level)
    (member? binding (car level))))

(define member?
  (lambda (item l)
    (cond
      ((null? l) #f)
      ((eq? (car l) item) #t)
      (else (member? item (cdr l))))))

(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

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
