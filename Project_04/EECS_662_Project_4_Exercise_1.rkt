#lang plai

#! Megan Teahan
#! 2617777
#! EECS 662 Project 4 Part 1
#! November 10th, 2015

#! CFWAES
(define-type CFWAES
  (num (n number?))
  (binop (name op?) (lhs CFWAES?) (rhs CFWAES?))
  (app (fun-expr CFWAES?) (expr CFWAES?))
  (fun (id symbol?) (body CFWAES?))
  (if0 (c CFWAES?) (t CFWAES?) (e CFWAES?))
  (with (bind binding?) (body CFWAES?))
  (seq (expr0 CFWAES?) (expr1 CFWAES?))
  (assign (x symbol?) (expr CFWAES?))
  (id (name symbol?))
  (op (name symbol?))
)

#! CFWAES Value
(define-type CFWAES-Value
  (numV (n number?))
  (closureV (param symbol?) (body CFWAES?) (ds DefrdSub?)))
        
#! Value and Store
(define-type ValueXStore
  (vxs (value CFWAES-Value?) (store Store?)))

#! Binding - for with
(define-type binding
  (with-binding (id symbol?) (bind-expr CFWAES?))
 )
        
#! Deferred Sub
(define-type DefrdSub
  (mtsub)
  (aSub (name symbol?) (value CFWAES-Value?) (ds DefrdSub?))
  )

#! Store
(define-type Store
  (mtsto)
  (aSto (location number?) (value CFWAES-Value?) (store Store?))
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
(define interp-cfwaes
  (lambda (cfwaes ds sto)
    (type-case CFWAES cfwaes
      (num (n) (vxs (numV n)))
      (binop (bo l r) 
             (type-case ValueXStore (interp-cfwaes l ds sto)
               (vxs (v1 s1)
                    (type-case ValueXStore (interp-cfwaes r ds s1)
                      (vxs (v2 s2)
                           (vxs (numV (check-arith-error (lookup (op-name bo) binop-table) v1 v2 ))))))))
      (app (fun_expr arg_expr)
           (let ([fun_val (interp-cfwaes fun_expr ds sto)])
             (if (numV? fun_val) 
                 (error 'interp-cfwaes "not a function/can't apply to number")
                 (interp-cfwaes (closureV-body fun_val) 
                              (aSub (closureV-param fun_val)
                                    (interp-cfwaes arg_expr ds)
                                    (closureV-ds fun_val)) sto)
             )))
      (if0 (c t e ) (if (equal? (numV 0) (interp-cfwaes c ds sto))
                        (interp-cfwaes t ds sto)
                        (interp-cfwaes e ds sto)))
      (with (bind body) 
            (interp-cfwaes (app (fun (with-binding-id bind) body) (with-binding-bind-expr bind)) ds sto))
      (fun (id body) (closureV id body ds))
      (assign (a c) (numV 1))
      (seq (e0 e1) 
           (type-case ValueXStore (interp-cfwaes e0 ds sto)
             (vxs (v_e0 s_e0)
                  (interp-cfwae e1 ds s_e0))))
      (op (o) o)
      (id (v)(lookup-ds v ds))
      )
    )
  )

#! Sets the next memory location
(define next-location
  (local ((define loc (box 0)))
    (lambda ()
      (begin (set-box! loc (+ (unbox loc) 1))
             (unbox loc)))))


#! checks to make sure not apply arithmetic to a function
(define check-arith-error
  (lambda (oper l r)
    (if (numV? (and l r))
        (oper (numV-n l) (numV-n r))
        (error 'interp-cfwaes "cannot perform arithmetic on functions")
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
;(test (interp-cfwaes (num 1) (mtsub) (mtsto)) (numV 1)) ;num
;(test (interp-cfwaes (binop (op 'add) (num 1) (num 2)) (mtsub) (mtsto)) (numV 3)) ;binop
;(test (interp-cfwaes (fun 'x (binop (op 'add) (num 1) (id 'x))) (mtsub) (mtsto)) (closureV 'x (binop (op 'add) (num 1) (id 'x)) (mtsub))) ;fun
;(test (interp-cfwaes (if0 (binop (op 'sub) (num 1) (num 1)) (num 3) (num 4)) (mtsub) (mtsto)) (numV 3)) ;if0
; (test (interp-cfwaes (app (fun 'x (binop (op 'add) (num 1) (id 'x))) (num 2)) (mtsub) (mtsto)) (numV 3)) ;app
; (test (interp-cfwaes (with (with-binding 'x (num 2)) (binop (op 'add) (num 1) (id 'x))) (mtsub) (mtsto)) (numV 3)) ;with
;(test (interp-cfwaes (if0 (num 0) (if0 (num 1) (num 3) (num 4)) (num 5)) (mtsub) (mtsto)) (numV 4)) ;nested ifs
; (test (interp-cfwaes (app (fun 'y (binop (op 'add) (id 'y) (num 1))) (app (fun 'x (binop (op 'add) (id 'x) (num 1))) (num 1))) (mtsub) (mtsto)) (numV 3)) ;nested app
;errors
;(test/exn (interp-cfwaes (id 'x) (mtsub) (mtsto)) "lookup: undefined id") ;id 
;(test/exn (interp-cfwaes (binop (op 'add) (num 1) (fun 'x (binop (op 'add) (num 1) (id 'x)))) (mtsub) (mtsto)) "cannot perform arithmetic on functions") ;can't apply arithmetic on function
;(test/exn (interp-cfwaes (app (num 1) (num 4)) (mtsub) (mtsto)) "not a function/can't apply to number") ;can't apply to number