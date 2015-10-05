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

#! Binding
(define-type binding
  (binded (name symbol?) (bind_expr CFAE?))
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
         ((fun) (fun (parse-cfae (cadr s_expr))
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

#! parses the bindings for with and puts them into a list
(define parse-bindings
  (lambda (binds)
    (if (symbol? (car binds))
        (list (parse-bind binds))
        (map parse-bind binds)
        )
    )
  )

#! puts all binding information in a binding type
(define parse-bind
  (lambda (bind)
    (binded (car bind) (parse-cfae (cadr bind)))
    )
  )
        
                 
#! evaluates input
(define eval-cfae
  (lambda (s_expr)
    (interp-cfae (parse-cfae s_expr))
    )
  )

#! checks to make sure there are no duplicate identifiers in the bindings
(define no-copies? 
  (lambda (bindings)
    (let loop ((b (car bindings)) (bs (cdr bindings)))
      (cond 
        ((empty? bs) true)
        ((no-copies-helper b bs) false)
        (else (loop (car bs) (cdr bs)))
        )
      )
    )
  )

#! checks a current identifier against the other bindings in the list to check for copies
(define no-copies-helper
  (lambda (b bs)
    (cond 
      ((empty? bs) false)
      ((symbol=? (binded-name b) (binded-name (car bs))) true)
      (else (no-copies-helper b (cdr bs)))
      )
    )
  )

#! interpreter 
(define interp-cfae
  (lambda (cfae ds)
    (type-case CFAE cfae
      (num (n) n)
      (binop (bo l r) ((lookup (op-name bo) binop-table) (interp-cfae l) (interp-cfae r)))
      (app (fun_expr arg_expr)
           (let ([fun_val (interp-cfae fun_expr)])
             (interp-cfae (subst-cfae (fun-body fun_val)
                                 (fun-id fun_val)
                                 (interp-cfae arg_expr)))))
      (fun (id body) (interp-cfae (subst-cfae 
      (op (o) o)
      (id (v) (error 'interp-cfae "free identifier"))
      )
    )
  )

#! 
(define subst-cfae
  (lambda (bindings body)
    (foldl unbind body bindings)
    )
  )

#! Breaks down the binding to be able to put through the subst
(define unbind
  (lambda (bind body)
    (subst body (binded-name bind) (num (interp-cfae (binded-bind_expr bind))))
    )
  )

#! check ids to see if they match or not
(define check-ids 
  (lambda (sub_id bindings)
    (not (andmap (lambda (bind)
              (if (symbol=? sub_id (binded-name bind))
                  false
                  true
                  )
              ) bindings)
    )
    )
  )

#! gets binding informatoin to pass onto subst
(define subst-bindings 
  (lambda (sub_id val bindings)
    (map (lambda (bind)
           (binded (binded-name bind) (subst (binded-bind_expr bind) sub_id val)))
         bindings)
    )
  )
            
       
(define subst
  (lambda (expr sub_id val)
    (type-case WAEE expr
      (num (n) (num n))
      (binop (bo l r) (binop bo
                             (subst l sub_id val)
                             (subst r sub_id val)))
      (with (bindings bound_body)
            (if (check-ids sub_id bindings) 
                (with (subst-bindings sub_id val bindings) bound_body)
                (with (subst-bindings sub_id val bindings) (subst bound_body sub_id val))
                )
            )
      (id (v) 
          (if (symbol=? v sub_id)
              val
              (id v)))
      (op (o) (op o))
      )
    )
  )