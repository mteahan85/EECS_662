#lang plai

#! Megan Teahan
#! 2617777
#! EECS 662 Project 2 Part 2
#! October 8th, 2015

#! CFAE
(define-type CFAE
  (num (n number?))
  (binop (name op?) (lhs CFAE?) (rhs CFAE?))
  (app (fun-expr CFAE?) (arg-expr CFAE?))
  (fun (id symbol?) (body CFAE?))
  (if0 (c CFAE?) (t CFAE?) (e CFAE?))
  (id (name symbol?))
  (op (name symbol?))
)

#! CFWAE
(define-type CFWAE
  (wnum (n number?))
  (wbinop (name wop?) (lhs CFWAE?) (rhs CFWAE?))
  (wapp (fun-expr CFWAE?) (arg-expr CFWAE?))
  (wfun (id symbol?) (body CFWAE?))
  (wif0 (c CFWAE?) (t CFWAE?) (e CFWAE?))
  (wwith (expr binding?) (body CFWAE?))
  (wcond0 (conds list?) (ed CFWAE?))
  (wid (name symbol?))
  (wop (name symbol?))
)

#! Binding - for with & cond
(define-type binding
  (with-bind (id symbol?) (bind-expr CFWAE?))
  (cond-bind (bind-c CFWAE?) (bind-e CFWAE?))
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

(define prelude
  (aSub 'pi (num 3.14159)
        (aSub 'area (fun 'x (binop (op 'mult) (id 'pi) (binop (op 'mult) (id 'x) (id 'x))))
              (aSub 'inc (fun 'x (binop (op 'add) (id 'x) (num 1))) (mtsub)))))
   

#! Looks up the symbol/operator corresponding to the op-name
(define lookup
  (lambda (op-name op-table)
     (cond ((empty? op-table) (error 'lookup "Operator not found"))
           (else (if (symbol=? (binopr-name (car op-table)) op-name)
                     (binopr-op (car op-table))
                     (lookup op-name (cdr op-table)))))))

#! Elaborator that turns CFWAE to CFAE
(define elab-cfwae
  (lambda (e)
    (type-case CFWAE e
      (wnum (n) (num n))
      (wbinop (oper l r) (binop (elab-cfwae oper) (elab-cfwae l) (elab-cfwae r)))
      (wapp (fun_expr arg_expr) (app (elab-cfwae fun_expr) (elab-cfwae arg_expr)))
      (wfun (id body) (fun id (elab-cfwae body)))
      (wwith (bind body) (app (fun (with-bind-id bind) (elab-cfwae body)) (elab-cfwae (with-bind-bind-expr bind))))
      (wif0 (c t e) (if0 (elab-cfwae c) (elab-cfwae t) (elab-cfwae e)))
      (wcond0 (conds ed) (create-if0 conds ed))
      (wid (v) (id v))
      (wop (o) (op o))
      )
    )
  )
       
#! Transform CFWAE cond to CFAE nest if0 statement
(define create-if0
  (lambda (conds ed) ;list
    (if (empty? conds)
        (elab-cfwae ed)
        (type-case binding (car conds)
          (cond-bind (c e) (if0 (elab-cfwae c) (elab-cfwae e) (create-if0 (cdr conds) ed)))
          (with-bind (i e) e)))
    )
  )

#! evaluates input
(define eval-cfae
  (lambda (s_expr)
    (interp-cfae (elab-cfwae s_expr) prelude) ;; in excerise 2 call elab before interpreter 
    )
  )

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
(test (eval-cfae (wnum 1)) (num 1)) ;num
(test (elab-cfwae (wid 'x)) (id 'x)) ;id
(test (eval-cfae (wbinop (wop 'add) (wnum 1) (wnum 2))) (num 3)) ;binop
(test (eval-cfae (wfun 'x (wbinop (wop 'add) (wnum 1) (wid 'x)))) (fun 'x (binop (op 'add) (num 1) (id 'x)))) ;fun
(test (eval-cfae (wif0 (wbinop (wop 'sub) (wnum 1) (wnum 1)) (wnum 3) (wnum 4))) (num 3)) ;if0
(test (eval-cfae (wapp (wfun 'x (wbinop (wop 'add) (wnum 1) (wid 'x))) (wnum 2))) (num 3)) ;app
(test (eval-cfae (wif0 (wnum 0) (wif0 (wnum 1) (wnum 3) (wnum 4)) (wnum 5))) (num 4)) ;nested ifs
(test (eval-cfae (wapp (wfun 'y (wbinop (wop 'add) (wid 'y) (wnum 1))) (wapp (wfun 'x (wbinop (wop 'add) (wid 'x) (wnum 1))) (wnum 1)))) (num 3)) ;nested app
(test (eval-cfae (wcond0 (list (cond-bind (wnum 1) (wnum 3)) (cond-bind (wbinop (wop 'sub) (wnum 1) (wnum 1)) (wnum 4))) (wnum 5))) (num 4)) ;cond0
(test (eval-cfae (wcond0 '() (wnum 5))) (num 5)) ;cond0 with only ed
(test (eval-cfae (wcond0 '() (wif0 (wbinop (wop 'sub) (wnum 1) (wnum 1)) (wnum 3) (wnum 4)))) (num 3)) ;if0 within cond0
(test (eval-cfae (wwith (with-bind 'x (wnum 1)) (wbinop (wop 'add) (wnum 1) (wid 'x)))) (num 2)) ;with  
(test (eval-cfae (wwith (with-bind 'x (wnum 1)) (wwith (with-bind 'y (wnum 1)) (wbinop (wop 'add) (wid 'y) (wid 'x))))) (num 2)) ;nested with
(test (eval-cfae (wapp (wfun 'x (wid 'x)) (wid 'pi))) (num 3.14159)) ;pi
(test (eval-cfae (wapp (wid 'inc) (wnum 1))) (num 2)) ;inc
(test (eval-cfae (wapp (wid 'area) (wnum 2))) (num 12.56636)) ;area
;errors
(test/exn (eval-cfae (wid 'x)) "lookup: undefined id") ;id 
(test/exn (eval-cfae (wbinop (wop 'add) (wnum 1) (wfun 'x (wbinop (wop 'add) (wnum 1) (wid 'x))))) "cannot perform arithmetic on functions") ;can't apply arithmetic on function
(test/exn (eval-cfae (wapp (wnum 1) (wnum 4))) "not a function/can't apply to number") ;can't apply to number