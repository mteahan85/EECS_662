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

#! Parses input
(define parse-cfae
  (lambda (s_expr)
    (cond
      ((number? s_expr) (num s_expr))
      ((symbol? s_expr) (id s_expr))
      ((list? s_expr)
       (case (car s_expr)
         ((+) (binop
               (op `add)
               (parse-cfae (cadr s_expr))
               (parse-cfae (caddr s_expr))))
         ((-) (binop
               (op `sub) 
               (parse-cfae (cadr s_expr))
               (parse-cfae (caddr s_expr))))
         ((*) (binop
               (op `mult)
               (parse-cfae (cadr s_expr))
               (parse-cfae (caddr s_expr))))
         ((/) (binop
               (op `div)
               (parse-cfae (cadr s_expr))
               (parse-cfae (caddr s_expr))))
         ((fun) (fun (cadr s_expr)
                     (parse-cfae (caddr s_expr))))
         ((app) (app (parse-cfae (cadr s_expr))
                     (parse-cfae (caddr s_expr))))
         ((if0) (if0 (parse-cfae (cadr s_expr))
                     (parse-cfae (caddr s_expr))
                     (parse-cfae (cadddr s_expr))))
         (else (error "exhausted parser")))
       )
      (error "exhausted parser")
      )
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
      (if0 (c t e ) (if (eq? (num 0) (interp-cfae c ds))
                        (interp-cfae t)
                        (interp-cfae e)))
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


    
;(parse-cfae '(app (fun x (+ 1 x)) 3))
;(eval-cfae '(app (fun x (+ 1 x)) 3))
(eval-cfae '(+ 3 (fun x x)))
(eval-cfae '(+ (fun x x) (fun x x)))