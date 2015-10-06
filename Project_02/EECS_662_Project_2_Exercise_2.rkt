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
  (wcond0 (conds list?) (def CFWAE?))
  (wid (name symbol?))
  (wop (name symbol?))
)

#! Binding
(define-type binding
  (binded (id symbol?) (bind-expr CFWAE?))
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

#! Elaborator that turns CFWAE to CFAE
(define elab-cfwae
  (lambda (e)
    (type-case CFWAE e
      (wnum (n) (num n))
      (wbinop (oper l r) (binop (elab-cfwae oper) (elab-cfwae l) (elab-cfwae r)))
      (wapp (fun_expr arg_expr) (app (elab-cfwae fun_expr) (elab-cfwae arg_expr)))
      (wfun (id body) (fun id (elab-cfwae body)))
      (wwith (bind body) (app (fun (binded-id bind) (elab-cfwae body)) (elab-cfwae (binded-bind-expr bind))))
      (wif0 (c t e) (if0 (elab-cfwae c) (elab-cfwae t) (elab-cfwae e)))
      (wcond0 (conds def) (if0 (create-ifs conds) (elab-cfwae)))
      (wid (v) (id v))
      (wop (o) (op o))
      )
    )
  )
        
(define create-ifs
  (lambda (conds)
    conds ;;will need to decide on structure for conds to come in as. maybe a list of pairs?
          ;-- also will need to understand what is different between a prelude and a regular definition is. Is it defining a function in an abstract syntax and storing that list in a table?
    )
  )

#! evaluates input
(define eval-cfae
  (lambda (s_expr)
    (interp-cfae (parse-cfae s_expr) (mtsub)) ;; in excerise 2 call elab before interpreter 
    )
  )

;;excerise 2
;; two grammaers 1. WCFAE 2.CFAE. Elab converts WCFAE to CFAE. WFAE: with => CFAE: app. WFAE: cond0 => CFAE: if0. No parsing. WCFAE is an abstract syntax.

#! interpreter 
(define interp-cfae
  (lambda (cfae ds)
    (type-case CFAE cfae
      (num (n) (num n))
      (binop (bo l r) (num ((lookup (op-name bo) binop-table) (num-n (interp-cfae l ds)) (num-n (interp-cfae r ds)))))
      (app (fun_expr arg_expr)
           (let ([fun_val (interp-cfae fun_expr ds)])
             (if (fun? fun_val)
             (interp-cfae (fun-body fun_val)
                          (aSub (fun-id fun_val)
                                (interp-cfae arg_expr ds)
                                ds))
             (error 'interp-cfae "not a function")
             )))
      (if0 (c t e ) (if (eq? (num 0) (interp-cfae c ds))
                        (interp-cfae t)
                        (interp-cfae e)))
      (fun (id body) (fun id body))
      (op (o) o)
      (id (v)(lookup-ds v ds))
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
          
    
(parse-cfae '(app (fun x (+ 1 x)) 3))
(eval-cfae '(app (fun x (+ 1 x)) 3))