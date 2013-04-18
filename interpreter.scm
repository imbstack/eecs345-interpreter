; Interpreter Project, Part 4
; Tom Dooner (ted27)
; Brian Stack (bis12)

(load "lib/classParser.scm")
(load "environment.scm")
(load "functions.scm")
(load "classes.scm")

(define the-begin-environment
  (add-to-environment 'true #t
    (add-to-environment 'false #f
      (add-to-environment 'return 'None '((() ()))
))))

; The heart of it all. Creates a default environment that is used, and at the
; end of interpreting the list of things, returns whatever value is put in the
; "return" variable in the environment
(define interpret
  (lambda (filename class)
    (let (
          (env (interpret-global-statement-list (parser filename) the-begin-environment))
          (class-name (string->symbol class))
         )
      ;(display "okay, executing main. the environment is:") (pretty-print env) (display "\n")
      ;(display (get-environment 'main (get-class (string->symbol class) env))) ; debug
      (display (true-or-falsify
        (call-function 'main '() env class-name)))
    )
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
      ((eq? (car stmt) 'class) (interpret-class (cadr stmt) (caddr stmt) (cadddr stmt) env))
      ;(else (interpret-stmt (car stmt) env))
      (else (error "You can't have anything at the global level except classes"))
)))

; Interpret all of the statements (hopefully)
(define interpret-statement-list
  (lambda (parsetree env class object)
    ;(begin (display "\n") (display "interpreting ") (display parsetree) (display "\n")
    (cond
      ((null? parsetree) env)
      (else
        ; This passes the updated environment into the next statement to
        ; be interpreted
        (interpret-statement-list (cdr parsetree) (interpret-stmt (car parsetree) env class object) class object)))));)

; Returns the new environment after the stmt is interpreted.
(define interpret-stmt
  (lambda (stmt env class object)
    (cond
      ((number? stmt) env)
      ((atom? stmt) env)

      ; unary operations like (- (= x 3)) and (! (== (= x 3) y))
      ((or (eq? (car stmt) '!)
           (and (eq? (car stmt) '-) (null? (cddr stmt))))
       (interpret-stmt (cadr stmt) env class object))

      ; binary operation like (+ (= x 2) 3)
      ((or (member? (car stmt) '(+ - * / % -))
           (boolean-stmt? stmt))
       (interpret-stmt (caddr stmt) (interpret-stmt (cadr stmt) env class object) class object))

      ((eq? (car stmt) 'begin) (interpret-block stmt env class object))
      ((eq? (car stmt) 'while) (call/cc (lambda (break) (interpret-while stmt (add-to-environment 'break break (add-layer env)) class object))))
      ((eq? (car stmt) 'break) ((get-environment 'break env) (del-layer (del-layer env))))
      ((eq? (car stmt) 'continue) ((get-environment 'continue env) env))
      ((eq? (car stmt) 'var) (interpret-declare stmt env class object))
      ((eq? (car stmt) '=) (interpret-assign stmt env class object))
      ((eq? (car stmt) 'if) (interpret-branch stmt env class object))
      ((eq? (car stmt) 'return) (interpret-ret stmt env class object))
      ((eq? (car stmt) 'funcall) (begin (interpret-function-in-class stmt env class object) env))
)))

; Performs the actions in a block
(define interpret-block
  (lambda (stmt env class object)
    (del-layer
      (call/cc
        (lambda (cont)
          (interpret-statement-list (cdr stmt) (add-to-environment 'continue cont (add-layer env)) class object))))))

; Returns the value of the stmt based on the environment that is passed in.
(define interpret-stmt-value
  (lambda (stmt env class object)
    (cond
      ((null? stmt) 'None)
      ((number? stmt) stmt)
      ((atom? stmt) (get-environment stmt env))
      ((eq? (car stmt) 'dot) (interpret-dot-value (cadr stmt) (caddr stmt) env))
      ((eq? (car stmt) 'funcall) (interpret-function-in-class stmt env class object))
      ((eq? (car stmt) '=) (interpret-assign-value stmt env class object))
      ((eq? (car stmt) '+) ((interpret-binary +) stmt env class object))
      ((eq? (car stmt) '-) (interpret-negative stmt env class object))
      ((eq? (car stmt) '*) ((interpret-binary *) stmt env class object))
      ((eq? (car stmt) '/) ((interpret-binary (lambda (x y) (floor (/ x y)))) stmt env class object))
      ((eq? (car stmt) '%) ((interpret-binary remainder) stmt env class object))
      ((boolean-stmt? stmt) (interpret-bool-value stmt env class object))
)))

; Handles '(return x)
; Returns updated environment
(define interpret-ret
  (lambda (stmt env class object)
    ((get-environment 'returnfunc env) (update-environment 'return (interpret-stmt-value (cadr stmt) env class object) env))))

; Handles '(if (...) (...) (...))
; Returns updated environment
(define interpret-branch
  (lambda (stmt env class object)
    (if (interpret-bool-value (cadr stmt) env class object)
      ; if true
      (interpret-stmt (caddr stmt) env class object)
      ; if false
      (if (null? (cdddr stmt))
        env   ; <- if there is no "else"
        (interpret-stmt (cadddr stmt) env class object)))))

; Handles while loops
; Returns updated environment
(define interpret-while
  (lambda (stmt env class object)
    (if (interpret-bool-value (cadr stmt) env class object)
      (interpret-while stmt (interpret-stmt (caddr stmt) env class object) class object)
      env)))

; Handles '(> (= x (+ x 1)) y)
; Returns #t or #f based on the environment and the variables
(define interpret-bool-value
  (lambda (stmt env class object)
    (cond
      ((atom? stmt) (get-environment stmt env))
      ((eq? (car stmt) '==) ((interpret-binary eq?) stmt env class object))
      ((eq? (car stmt) '!=) ((interpret-binary (lambda (x y) (not (eq? x y)))) stmt env class object))
      ((eq? (car stmt) '<) ((interpret-binary <) stmt env class object))
      ((eq? (car stmt) '>) ((interpret-binary >) stmt env class object))
      ((eq? (car stmt) '<=) ((interpret-binary <=) stmt env class object))
      ((eq? (car stmt) '>=) ((interpret-binary >=) stmt env class object))
      ((eq? (car stmt) '&&) ((interpret-binary (lambda (x y) (and x y))) stmt env class object))
      ((eq? (car stmt) '||) ((interpret-binary (lambda (x y) (or x y))) stmt env class object))
      ((eq? (car stmt) '!) (interpret-not stmt env class object))
)))

; Handles '(var x)
; Returns updated environment
(define interpret-declare
  (lambda (stmt env class object)
    (cond
      ((null? (cddr stmt)) (add-to-environment (cadr stmt) 'None env))
      (else (add-to-environment (cadr stmt) (interpret-stmt-value (caddr stmt) env class object) env)))))

; Handles '(= x (+ x 1))
; Returns updated environment
(define interpret-assign
  (lambda (stmt env class object)
    (update-environment (cadr stmt) (interpret-stmt-value (caddr stmt) env class object) env)))

; Handles '(= x (+ x 1))
; Returns value, which is just the value of the caddr of it.
(define interpret-assign-value
  (lambda (stmt env class object)
    (cond
      ((number? (caddr stmt)) (get-environment (cadr stmt) (update-environment (cadr stmt) (caddr stmt) env)))
      (else (get-environment (cadr stmt) (update-environment (cadr stmt) (interpret-stmt-value (caddr stmt) env class object) env))))))

; Helper function to abstract out binary operations like < > + - && ||, etc.
(define interpret-binary
  (lambda (op)
    (lambda (stmt env class object)
      (op
        (interpret-stmt-value (cadr stmt) env class object)
        (interpret-stmt-value (caddr stmt) env class object)))))

; Helper function for unary or binary subtraction
(define interpret-negative
  (lambda (stmt env class object)
    (cond
      ((eq? (length stmt) 3) ((interpret-binary -) stmt env class object))
      (else (- 0 (interpret-stmt-value (cadr stmt) env class object))))))

; Helper function for unary !
(define interpret-not
  (lambda (stmt env class object)
    (not (interpret-stmt-value (cadr stmt) env class object))))

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
