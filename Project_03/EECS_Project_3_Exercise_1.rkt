#lang plai

#! Megan Teahan
#! 2617777
#! EECS 662 Project 3 Part 1
#! November 10th, 2015

#! CFWAE
(define-type CFWAE
  (num (n number?))
  (binop (name op?) (lhs CFWAE?) (rhs CFWAE?))
  (app (fun-expr CFWAE?) (expr CFWAE?))
  (fun (id symbol?) (body CFWAE?))
  (if0 (c CFWAE?) (t CFWAE?) (e CFWAE?))
  (with (bind binding?) (body CFWAE?))
  (id (name symbol?))
  (op (name symbol?))
)

#! CFWAE Value
(define-type Value
  (numV (n number?))
  (closureV (param symbol?) (body CFWAE?) (ds DefrdSub?)))
        
 
#! Binding - for with
(define-type binding
  (with-binding (id symbol?) (bind-expr CFWAE?))
 )
        
#! Deferred Sub
(define-type DefrdSub
  (mtsub)
  (aSub (name symbol?) (value Value?) (ds DefrdSub?))
  )
             
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

#! interpreter 
(define interp-cfwae
  (lambda (cfwae ds)
    (type-case CFWAE cfwae
      (num (n) (numV n))
      (binop (bo l r) (numV (check-arith-error (lookup (op-name bo) binop-table)  (interp-cfwae l ds)  (interp-cfwae r ds))))
      (app (fun_expr arg_expr)
           (let ([fun_val (interp-cfwae fun_expr ds)])
             (if (numV? fun_val) 
                 (error 'interp-cfwae "not a function/can't apply to number")
                 (interp-cfwae (closureV-body fun_val) 
                              (aSub (closureV-param fun_val)
                                    (interp-cfwae arg_expr ds)
                                    (closureV-ds fun_val)))
             )))
      (if0 (c t e ) (if (equal? (numV 0) (interp-cfwae c ds))
                        (interp-cfwae t ds)
                        (interp-cfwae e ds)))
      (with (bind body) 
            (interp-cfwae (app (fun (with-binding-id bind) body) (with-binding-bind-expr bind)) ds))
      (fun (id body) (closureV id body ds))
      (op (o) o)
      (id (v)(lookup-ds v ds))
      )
    )
  )

#! checks to make sure not apply arithmetic to a function
(define check-arith-error
  (lambda (oper l r)
    (if (numV? (and l r))
        (oper (numV-n l) (numV-n r))
        (error 'interp-cfwae "cannot perform arithmetic on functions")
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
      )
    )
  )

;cases
(test (interp-cfwae (num 1) (mtsub)) (numV 1)) ;num
(test (interp-cfwae (binop (op 'add) (num 1) (num 2)) (mtsub)) (numV 3)) ;binop
(test (interp-cfwae (fun 'x (binop (op 'add) (num 1) (id 'x))) (mtsub)) (closureV 'x (binop (op 'add) (num 1) (id 'x)) (mtsub))) ;fun
(test (interp-cfwae (if0 (binop (op 'sub) (num 1) (num 1)) (num 3) (num 4)) (mtsub)) (numV 3)) ;if0
(test (interp-cfwae (app (fun 'x (binop (op 'add) (num 1) (id 'x))) (num 2)) (mtsub)) (numV 3)) ;app
(test (interp-cfwae (with (with-binding 'x (num 2)) (binop (op 'add) (num 1) (id 'x))) (mtsub)) (numV 3)) ;with
(test (interp-cfwae (if0 (num 0) (if0 (num 1) (num 3) (num 4)) (num 5)) (mtsub)) (numV 4)) ;nested ifs
(test (interp-cfwae (app (fun 'y (binop (op 'add) (id 'y) (num 1))) (app (fun 'x (binop (op 'add) (id 'x) (num 1))) (num 1))) (mtsub)) (numV 3)) ;nested app
;errors
(test/exn (interp-cfwae (id 'x) (mtsub)) "lookup: undefined id") ;id 
(test/exn (interp-cfwae (binop (op 'add) (num 1) (fun 'x (binop (op 'add) (num 1) (id 'x)))) (mtsub)) "cannot perform arithmetic on functions") ;can't apply arithmetic on function
(test/exn (interp-cfwae (app (num 1) (num 4)) (mtsub)) "not a function/can't apply to number") ;can't apply to number
