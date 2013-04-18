; Interpreter Project, Part 4
; Tom Dooner (ted27)
; Brian Stack (bis12)

; interprets the class and returns an environment with the class added to it
; The class object contains:
;   [1] The environment of class (static) variables.
;   [2] The environment of instance variables, initialized to 'None
;   [3] Parent class name
;   (todo: also return "initial environment"?)
; e.g.
; class Main extends SomethingElse {
;   static var x = 5;
;   main() { return x; }
; }
; becomes...
; '((((main x true false return) (#&(() ((return x))) #&5 #&#t #&#f #&None)))
;   ()
;   SomethingElse)

(define interpret-class
  (lambda (name extends body env)
    (let*
      ((extends (if (null? extends) 'None (cadr extends))) ;just the name of the class
      (empty-class (cons '((()())) (cons '() (list extends)))) ; '((()()) () 'Parent)
      (env (add-to-environment name empty-class env)))

      ;(cons (interpret-class-body-list body env name 'noobjectsyet!)
      ;  (cons '()       ; no instance variables in this assignment!
      ;    (cons (if (null? extends) 'None (cadr extends)) '())))
      (interpret-class-body-list body env name 'noobjectsyet!)
    )
))

(define interpret-class-body-list
  (lambda (parsetree env class object)
    ;(display "interpreting class body list with env:") (pretty-print env) (display "\n")
    (cond
      ((null? parsetree) env)
      (else (interpret-class-body-list (cdr parsetree) (interpret-class-body (car parsetree) env class object) class object)))))

(define interpret-class-body
  (lambda (stmt env class object)
    (cond
      ((eq? (car stmt) 'static-function) (interpret-declare-function stmt env class object))
      ((eq? (car stmt) 'static-var) (interpret-declare-static-var stmt env class object))
)))

(define interpret-declare-static-var
  (lambda (stmt env class object)
    (let*
      (
       (class-env (cons (car (get-class-parsetree class env)) env))
       (binding (cadr stmt))
       (value (if (null? (cddr stmt)) 'None (interpret-stmt-value (caddr stmt) class-env class object)))
       (with-rest-of-class (lambda (v) (cons v (cdr (get-class class env)))))
      )
      ;(display "setting static var with env: ") (pretty-print (get-class-parsetree class class-env))
      (update-environment class (with-rest-of-class (add-to-environment binding value (get-class-parsetree class env))) env))))

(define get-class
  (lambda (classname env)
    (get-environment classname env)))

(define get-class-parsetree
  (lambda (classname env)
    (car (get-class classname env))))

; returns the *name* of the parent class:
(define get-class-parent
  (lambda (classname env)
    (caddr (get-class classname env))))

(define interpret-dot-value
  (lambda (class binding env)
    (get-environment binding (get-class-parsetree class env))))

(define interpret-function-in-class
  (lambda (stmt env class object)
    (if (list? (cadr stmt))
      ; if a (dot A f) part is provided
      (let*
        ((dot-expr (cadr stmt))
         (class-name (if (eq? (cadr dot-expr)'super) (get-class-parent class env) (cadr dot-expr)))
         (function-name (caddr dot-expr))
         (function-params (cddr stmt)))
        (call-function function-name function-params env class-name))
      ; if the function is called in the current class
      (call-function (cadr stmt) (cddr stmt) env 'Main))))
