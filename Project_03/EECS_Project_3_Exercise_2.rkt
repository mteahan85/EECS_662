#lang plai

#! Megan Teahan
#! 2617777
#! EECS 662 Project 3 Part 2
#! November 10th, 2015

#! CFWAER
(define-type CFWAER
  (num (n number?))
  (binop (name op?) (lhs CFWAER?) (rhs CFWAER?))
  (app (fun-expr CFWAER?) (expr CFWAER?))
  (fun (id symbol?) (body CFWAER?))
  (if0 (c CFWAER?) (t CFWAER?) (e CFWAER?))
  (with (bind binding?) (body CFWAER?))
  (rec (bind binding?) (body CFWAER?))
  (id (name symbol?))
  (op (name symbol?))
)

#! CFWAER Value
(define-type Value
  (numV (n number?))
  (closureV (param symbol?) (body CFWAER?) (ds DefrdSub?)))
        

#! Binding - for with & rec
(define-type binding
  (binding-item (id symbol?) (bind-expr CFWAER?))
 )
        
#! Deferred Sub
(define-type DefrdSub
  (mtsub)
  (aSub (name symbol?) (value Value?) (ds DefrdSub?))
  (aRecSub (name symbol?) (value boxed-CFWAER-value?) (ds DefrdSub?))
  )

#! Check CFWAER Value
(define boxed-CFWAER-value?
  (lambda (v)
    (and (box? v)
         (Value? (unbox v)))))  
             
#! Binop Record 
(define-type binop-rec
  (binopr (name symbol?) (op procedure?)))

(define binop-table
  (list 
    (binopr `add +)
    (binopr `sub -)
    (binopr `mult *)
    (binopr `div /)
    )
  )

#! Looks up the symbol/operator corresponding to the op-name
(define lookup
  (lambda (op-name op-table)
     (cond ((empty? op-table) (error 'lookup "Operator not found"))
           (else (if (symbol=? (binopr-name (car op-table)) op-name)
                     (binopr-op (car op-table))
                     (lookup op-name (cdr op-table)))))))



#! parser
(define parse-cfwaer
    (lambda (s_expr)
    (cond
      ((number? s_expr) (num s_expr))
      ((symbol? s_expr) (id s_expr))
      ((list? s_expr)
       (case (car s_expr)
         ((+) (binop
               (op `add)
               (parse-cfwaer (cadr s_expr))
               (parse-cfwaer (caddr s_expr))))
         ((-) (binop
               (op `sub) 
               (parse-cfwaer (cadr s_expr))
               (parse-cfwaer (caddr s_expr))))
         ((*) (binop
               (op `mult)
               (parse-cfwaer (cadr s_expr))
               (parse-cfwaer (caddr s_expr))))
         ((/) (binop
               (op `div)
               (parse-cfwaer (cadr s_expr))
               (parse-cfwaer (caddr s_expr))))
         ((with) (with (parse-bind (cadr s_expr))
                       (parse-cfwaer (caddr s_expr))))
         ((fun) (fun (cadr s_expr) 
                     (parse-cfwaer (caddr s_expr))))
         ((if0) (if0 (parse-cfwaer  (cadr s_expr))
                     (parse-cfwaer (caddr s_expr))
                     (parse-cfwaer (cadddr s_expr))))
         ((rec) (rec (parse-bind (cadr s_expr))
                  (parse-cfwaer (caddr s_expr))))
         (else (app (parse-cfwaer (car s_expr))
                  (parse-cfwaer (cadr s_expr)))))
       )
      (error "exhausted parser")
      )
    )
  ) 

#! parse binding
(define parse-bind
  (lambda (bind)
    (binding-item (car bind) (parse-cfwaer (cadr bind)))
    )
  )
#! interpreter 
(define interp-cfwaer
  (lambda (cfwaer ds)
    (type-case CFWAER cfwaer
      (num (n) (numV n))
      (binop (bo l r) (numV (check-arith-error (lookup (op-name bo) binop-table)  (interp-cfwaer l ds)  (interp-cfwaer r ds))))
      (app (fun_expr arg_expr)
           (let ([fun_val (interp-cfwaer fun_expr ds)])
             (if (numV? fun_val) 
                 (error 'interp-cfwaer "not a function/can't apply to number")
                 (interp-cfwaer (closureV-body fun_val) 
                              (aSub (closureV-param fun_val)
                                    (interp-cfwaer arg_expr ds)
                                    (closureV-ds fun_val)))
             )))
      (if0 (c t e ) (if (equal? (numV 0) (interp-cfwaer c ds))
                        (interp-cfwaer t ds)
                        (interp-cfwaer e ds)))
      (with (bind body) 
            (interp-cfwaer (app (fun (binding-item-id bind) body) (binding-item-bind-expr bind)) ds))
      (fun (id body) (closureV id body ds))
      (rec (bound_item bound_body)
        (interp-cfwaer bound_body (cyclically-bind-and-interp (binding-item-id bound_item) 
                                                              (binding-item-bind-expr bound_item)
                                                              ds)))
      (op (o) o)
      (id (v)(lookup-ds v ds))
      )
    )
  )

#! Recursive Binding
(define cyclically-bind-and-interp
  (lambda (bound_id named_expr ds)
    (letrec ((value_holder (box (numV 1729)))
             (new_env (aRecSub bound_id value_holder ds))
             (named_expr_val (interp-cfwaer named_expr new_env)))
      (begin 
        (set-box! value_holder named_expr_val)
        new_env))))



#! checks to make sure not apply arithmetic to a function
(define check-arith-error
  (lambda (oper l r)
    (if (numV? (and l r))
        (oper (numV-n l) (numV-n r))
        (error 'interp-cfwaer "cannot perform arithmetic on functions")
    )
  )
  )

#! Looks up variable in DefrdSub list      
(define lookup-ds
  (lambda (name ds)
    (type-case DefrdSub ds
      (mtsub () (error 'lookup "undefined id"))
      (aSub (bound_name bound_value rest_ds)
            (if (symbol=? name bound_name)
                bound_value
                (lookup-ds name rest_ds)
                )
            )
      (aRecSub (bound_name bound_value rest_ds)
               (if (symbol=? name bound_name)
                   (unbox bound_value)
                   (lookup-ds name rest_ds)))
      )
    )
  )



(define eval-cfwaer
  (lambda (expr)
    (interp-cfwaer (parse-cfwaer expr) (mtsub))))



;Perry Test Cases
(test (eval-cfwaer '{{fun x {+ 1 3}} 1}) (numV 4))
(test (eval-cfwaer '{with {y 1} {{fun x {+ y x}} 3}}) (numV 4))
(test (eval-cfwaer '{with {y 1} {with {f {fun x {+ y x}}} {f 3}}}) (numV 4))
(test (eval-cfwaer '{with {y 1} {with {f {fun x {+ y x}}} {with {y 100} {f 3}}}}) (numV 4))
(test (eval-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 0}}) (numV 1))
(test (eval-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 3}})(numV 6))
(test (eval-cfwaer '{rec {fac {fun x {if0 x 1 {* x {fac {- x 1}}}}}} {fac 5}})(numV 120))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 1} 1}})(numV 3))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 2} 2}})(numV 7))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 3} 3}})(numV 61))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 0} 3}})(numV 4))
(test (eval-cfwaer '{rec {ack {fun m {fun n {if0 m {+ n 1} {if0 n {{ack {- m 1}} 1} {{ack {- m 1}} {{ack m} {- n 1}}}}}}}} {{ack 3} 0}})(numV 5))
(test (eval-cfwaer '{{fun x {+ 1 3}} 1}) (numV 4))
(test (eval-cfwaer '{rec {y 1} {{fun x {+ y x}} 3}}) (numV 4))
(test (eval-cfwaer '{rec {y 1} {with {f {fun x {+ y x}}} {f 3}}}) (numV 4))
(test (eval-cfwaer '{with {y 1} {rec {f {fun x {+ y x}}} {with {y 100} {f 3}}}}) (numV 4))
(test (eval-cfwaer '{rec {y 1} {rec {f {fun x {+ y x}}} {rec {y 100} {f 3}}}}) (numV 4))

;Test Cases
(test (interp-cfwaer (num 1) (mtsub)) (numV 1)) ;num
(test (interp-cfwaer (binop (op 'add) (num 1) (num 2)) (mtsub)) (numV 3)) ;binop
(test (interp-cfwaer (fun 'x (binop (op 'add) (num 1) (id 'x))) (mtsub)) (closureV 'x (binop (op 'add) (num 1) (id 'x)) (mtsub))) ;fun
(test (interp-cfwaer (if0 (binop (op 'sub) (num 1) (num 1)) (num 3) (num 4)) (mtsub)) (numV 3)) ;if0
(test (interp-cfwaer (app (fun 'x (binop (op 'add) (num 1) (id 'x))) (num 2)) (mtsub)) (numV 3)) ;app
(test (interp-cfwaer (with (binding-item 'x (num 2)) (binop (op 'add) (num 1) (id 'x))) (mtsub)) (numV 3)) ;with
(test (interp-cfwaer (if0 (num 0) (if0 (num 1) (num 3) (num 4)) (num 5)) (mtsub)) (numV 4)) ;nested ifs
(test (interp-cfwaer (app (fun 'y (binop (op 'add) (id 'y) (num 1))) (app (fun 'x (binop (op 'add) (id 'x) (num 1))) (num 1))) (mtsub)) (numV 3)) ;nested app
;errors
(test/exn (interp-cfwaer (id 'x) (mtsub)) "lookup: undefined id") ;id 
(test/exn (interp-cfwaer (binop (op 'add) (num 1) (fun 'x (binop (op 'add) (num 1) (id 'x)))) (mtsub)) "cannot perform arithmetic on functions") ;can't apply arithmetic on function
(test/exn (interp-cfwaer (app (num 1) (num 4)) (mtsub)) "not a function/can't apply to number") ;can't apply to number
