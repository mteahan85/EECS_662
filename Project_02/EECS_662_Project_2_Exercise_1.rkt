#lang plai

#! Megan Teahan
#! 2617777
#! EECS 662 Project 2 Part 1
#! October 8th, 2015

#! CFAE
(define-type CFAE
  (num (n number?))
  (binop (name op?) (lhs CFAE?) (rhs CFAE?))
  (app (fun-expr CFAE?) (expr CFAE?))
  (fun (id symbol?) (body CFAE?))
  (if0 (c CFAE?) (t CFAE?) (e CFAE?))
  (id (name symbol?))
  (op (name symbol?))
)

#! Deferred Sub
(define-type DefrdSub
  (mtsub)
  (aSub (name symbol?) (value CFAE?) (ds DefrdSub?))
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
(define interp-cfae
  (lambda (cfae ds)
    (type-case CFAE cfae
      (num (n) (num n))
      (binop (bo l r) (num (check-arith-error (lookup (op-name bo) binop-table)  (interp-cfae l ds)  (interp-cfae r ds))))
      (app (fun_expr arg_expr)
           (let ([fun_val (interp-cfae fun_expr ds)])
             (if (num? fun_val)
                 (error 'interp-cfae "not a function/can't apply to number")
                 (interp-cfae (fun-body fun_val)
                              (aSub (fun-id fun_val)
                                    (interp-cfae arg_expr ds)
                                    ds)))
             ))
      (if0 (c t e ) (if (equal? (num 0) (interp-cfae c ds))
                        (interp-cfae t ds)
                        (interp-cfae e ds)))
      (fun (id body) (fun id body))
      (op (o) o)
      (id (v)(lookup-ds v ds))
      )
    )
  )

#! checks to make sure not apply arithmetic to a function
(define check-arith-error
  (lambda (oper l r)
    (if (num? (and l r))
        (oper (num-n l) (num-n r))
        (error 'interp-cfae "cannot perform arithmetic on functions")
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
(test (interp-cfae (num 1) (mtsub)) (num 1)) ;num
(test (interp-cfae (binop (op 'add) (num 1) (num 2)) (mtsub)) (num 3)) ;binop
(test (interp-cfae (fun 'x (binop (op 'add) (num 1) (id 'x))) (mtsub)) (fun 'x (binop (op 'add) (num 1) (id 'x)))) ;fun
(test (interp-cfae (if0 (binop (op 'sub) (num 1) (num 1)) (num 3) (num 4)) (mtsub)) (num 3)) ;if0
(test (interp-cfae (app (fun 'x (binop (op 'add) (num 1) (id 'x))) (num 2)) (mtsub)) (num 3)) ;app
(test (interp-cfae (if0 (num 0) (if0 (num 1) (num 3) (num 4)) (num 5)) (mtsub)) (num 4)) ;nested ifs
(test (interp-cfae (app (fun 'y (binop (op 'add) (id 'y) (num 1))) (app (fun 'x (binop (op 'add) (id 'x) (num 1))) (num 1))) (mtsub)) (num 3)) ;nested app
;errors
(test/exn (interp-cfae (id 'x) (mtsub)) "lookup: undefined id") ;id 
(test/exn (interp-cfae (binop (op 'add) (num 1) (fun 'x (binop (op 'add) (num 1) (id 'x)))) (mtsub)) "cannot perform arithmetic on functions") ;can't apply arithmetic on function
(test/exn (interp-cfae (app (num 1) (num 4)) (mtsub)) "not a function/can't apply to number") ;can't apply to number