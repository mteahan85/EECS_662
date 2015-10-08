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
;;figure out structure for cond in CFWAE. Once figured out, easy to convert over to CFAE


#! Binding - for with
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
        

;(cond0 ()* (else 'item)


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

;;excerise 2
;; two grammaers 1. WCFAE 2.CFAE. Elab converts WCFAE to CFAE. WFAE: with => CFAE: app. WFAE: cond0 => CFAE: if0. No parsing. WCFAE is an abstract syntax.

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
 ;(if0 (num 0) (num 3) (if0 (num 0) (num 4) (num 5)))
;cond structure
;(cond '( (cond-bind (num 1) (num 3)) (cond-bind (num 2) (num 4)) (num 3))
;(lookup-ds 'area prelude)
;(interp-cfae (app (id 'inc) (id 'pi)) prelude)
(elab-cfwae (wcond0 (list (cond-bind (wnum 0) (wnum 3)) (cond-bind (wnum 0) (wnum 4))) (wnum 5)))
(interp-cfae (elab-cfwae (wcond0 (list (cond-bind (wnum 1) (wnum 3)) (cond-bind (wnum 1) (wnum 4))) (wnum 5))) prelude)
(eval-cfae (wcond0 (list (cond-bind (wnum 1) (wnum 3)) (cond-bind (wnum 1) (wnum 4))) (wnum 5)))