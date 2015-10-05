#lang plai

#! Megan Teahan
#! 2617777
#! EECS 662 Project 1 Part 1
#! September 15th, 2015


(define-type WAE
  (num (n number?))
  (add (lhs WAE?) (rhs WAE?))
  (sub (lhs WAE?) (rhs WAE?))
  (with (name symbol?) (name_expr WAE?) (body WAE?))
  (id (name symbol?))
)

(define parse-wae
  (lambda (s_expr)
    (cond
      ((number? s_expr) (num s_expr))
      ((symbol? s_expr) (id s_expr))
      ((list? s_expr)
       (case (car s_expr)
         ((+) (add (parse-wae (cadr s_expr))
                   (parse-wae (caddr s_expr))))
         ((-) (sub (parse-wae (cadr s_expr))
                   (parse-wae (caddr s_expr))))
         ((with) (with (caadr s_expr)
                       (parse-wae (cadadr s_expr))
                       (parse-wae (caddr s_expr))))
         
         (else (error "exhausted parser")))
       )
      (error "exhausted parser")
      )
    )
  )

(define eval-wae
  (lambda (s_expr)
    (interp-wae (parse-wae s_expr))
    )
  )

(define interp-wae
  (lambda (wae)
    (type-case WAE wae
      (num (n) n)
      (add (l r) (+ (interp-wae l) (interp-wae r)))
      (sub (l r) (- (interp-wae l) (interp-wae r)))
      (with (bound_id named_expr bound_body)
            (interp-wae (subst bound_body bound_id (num (interp-wae named_expr)))))
      (id (v) (error 'interp-wae "free identifier"))
      )
    )
  )


(define subst
  (lambda (expr sub_id val)
    (type-case WAE expr
      (num (n) (num n))
      (add (l r) (add (subst l sub_id val)
                      (subst r sub_id val)))
      (sub (l r) (sub (subst l sub_id val)
                      (subst r sub_id val)))
      (with (bound_id named_expr bound_body)
            (if (symbol=? bound_id sub_id)
                (with bound_id (subst named_expr sub_id val) bound_body)
                (with bound_id (subst named_expr sub_id val) 
                      (subst bound_body sub_id val))
                )
            )
      (id (v) 
          (if (symbol=? v sub_id)
              val
              (id v)))

      )
    )
  )
       