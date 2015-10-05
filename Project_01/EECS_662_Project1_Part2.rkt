#lang plai

#! Megan Teahan
#! 2617777
#! EECS 662 Project 1 Part 2
#! September 15th, 2015

#! WAEE
(define-type WAEE
  (num (n number?))
  (binop (name op?) (lhs WAEE?) (rhs WAEE?))
  (with (bindings list?) (body WAEE?))
  (id (name symbol?))
  (op (name symbol?))
)

#! Binding
(define-type binding
  (binded (name symbol?) (bind_expr WAEE?))
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
(define parse-waee
  (lambda (s_expr)
    (cond
      ((number? s_expr) (num s_expr))
      ((symbol? s_expr) (id s_expr))
      ((list? s_expr)
       (case (car s_expr)
         ((+) (binop
               (op `add)
               (parse-waee (cadr s_expr))
               (parse-waee (caddr s_expr))))
         ((-) (binop
               (op `sub) 
               (parse-waee (cadr s_expr))
               (parse-waee (caddr s_expr))))
         ((*) (binop
               (op `mult)
               (parse-waee (cadr s_expr))
               (parse-waee (caddr s_expr))))
         ((/) (binop
               (op `div)
               (parse-waee (cadr s_expr))
               (parse-waee (caddr s_expr))))
         ((with) (with (parse-bindings (cadr s_expr))
                       (parse-waee (caddr s_expr))))
         
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
    (binded (car bind) (parse-waee (cadr bind)))
    )
  )
        
                 
#! evaluates input
(define eval-waee
  (lambda (s_expr)
    (interp-waee (parse-waee s_expr))
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
(define interp-waee
  (lambda (waee)
    (type-case WAEE waee
      (num (n) n)
      (binop (bo l r) ((lookup (op-name bo) binop-table) (interp-waee l) (interp-waee r)))
      (with (bindings bound_body)
            (if (no-copies? bindings)
                (interp-waee (subst-waee bindings bound_body))
                (error 'interp-waee "duplicate binding")))
      (op (o) o)
      (id (v) (error 'interp-waee "free identifier"))
      )
    )
  )

#! 
(define subst-waee
  (lambda (bindings body)
    (foldl unbind body bindings)
    )
  )

#! Breaks down the binding to be able to put through the subst
(define unbind
  (lambda (bind body)
    (subst body (binded-name bind) (num (interp-waee (binded-bind_expr bind))))
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
