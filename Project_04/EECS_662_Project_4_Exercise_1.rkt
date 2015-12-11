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
  (closureV (param symbol?) (body CFWAES?) (ds DefrdSub?))
  (boxV (location number?))) ;CFWAES-Value? potentially
        
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
  (aSub (name symbol?) (location number?) (ds DefrdSub?)) ;; should hold location not value
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


#! parser
(define parse-cfwaes
    (lambda (s_expr)
    (cond
      ((number? s_expr) (num s_expr))
      ((symbol? s_expr) (id s_expr))
      ((list? s_expr)
       (case (car s_expr)
         ((+) (binop
               (op `add)
               (parse-cfwaes (cadr s_expr))
               (parse-cfwaes (caddr s_expr))))
         ((-) (binop
               (op `sub) 
               (parse-cfwaes (cadr s_expr))
               (parse-cfwaes (caddr s_expr))))
         ((*) (binop
               (op `mult)
               (parse-cfwaes (cadr s_expr))
               (parse-cfwaes (caddr s_expr))))
         ((/) (binop
               (op `div)
               (parse-cfwaes (cadr s_expr))
               (parse-cfwaes (caddr s_expr))))
         ((with) (with (parse-bind (cadr s_expr))
                       (parse-cfwaes (caddr s_expr))))
         ((fun) (fun (cadr s_expr) 
                     (parse-cfwaes (caddr s_expr))))
         ((if0) (if0 (parse-cfwaes  (cadr s_expr))
                     (parse-cfwaes (caddr s_expr))
                     (parse-cfwaes (cadddr s_expr))))
         ((seq) (seq (parse-cfwaes (cadr s_expr))
                     (parse-cfwaes (caddr s_expr))))
         ((assign) (assign (cadr s_expr)
                           (parse-cfwaes (caddr s_expr))))
         (else (app (parse-cfwaes (car s_expr))
                  (parse-cfwaes (cadr s_expr)))))
       )
      (error "exhausted parser")
      )
    )
  ) 

#! parse binding
(define parse-bind
  (lambda (bind)
    (with-binding (car bind) (parse-cfwaes (cadr bind)))
    )
  )


#! interpreter 
(define interp-cfwaes
  (lambda (cfwaes ds sto)
    (type-case CFWAES cfwaes
      (num (n) (vxs (numV n) sto))
      (binop (bo l r) 
             (type-case ValueXStore (interp-cfwaes l ds sto)
               (vxs (v1 s1)
                    (type-case ValueXStore (interp-cfwaes r ds s1) 
                      (vxs (v2 s2)
                           (vxs (numV (check-arith-error (lookup (op-name bo) binop-table) v1 v2 )) s2))))))
      (app (fun_expr arg_expr)
           (let ([fun_val (interp-cfwaes fun_expr ds sto)])
             (if (numV? (vxs-value fun_val)) 
                 (error 'interp-cfwaes "not a function/can't apply to number")
                 (let([new_loc (next-location)])
                   (let ([arg_val (interp-cfwaes arg_expr ds sto)])
                   (interp-cfwaes (closureV-body (vxs-value fun_val)) 
                                  (aSub (closureV-param (vxs-value fun_val))
                                        new_loc
                                        (closureV-ds (vxs-value fun_val)))
                                  (aSto new_loc
                                        (vxs-value arg_val)
                                        (vxs-store arg_val)
                                 ))))
             )))
      (if0 (c t e ) (if (equal? (numV 0) (vxs-value (interp-cfwaes c ds sto))) ;; not evaulating correctly 
                        (interp-cfwaes t ds sto)
                        (interp-cfwaes e ds sto)))
      (with (bind body) 
            (interp-cfwaes (app (fun (with-binding-id bind) body) (with-binding-bind-expr bind)) ds sto))
      (fun (id body) (vxs (closureV id body ds) sto))
      (assign (x expr) 
              (type-case ValueXStore (interp-cfwaes expr ds sto)
                (vxs (v1 s1)
                     (vxs  v1 (aSto (lookup-loc x ds)
                                    v1
                                    s1))))) ;?? not returning a vxs
      (seq (e0 e1) 
           (type-case ValueXStore (interp-cfwaes e0 ds sto)
             (vxs (v_e0 s_e0)
                  (interp-cfwaes e1 ds s_e0))))
      (op (o) o)
      (id (v)(lookup-value v ds sto))
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

#! Looks up value
(define lookup-value
  (lambda (name ds sto)
    (type-case DefrdSub ds
      (mtsub () (error 'lookup "undefined id"))
      (aSub (bound_name bound_value rest_ds)
            (if (symbol=? name bound_name) ;;issued caused somewhere in here -- returning the locatoin and not the value
                (lookup-sto bound_value sto sto)
                (lookup-value name rest_ds sto)
                )
            )
      )
    )
  )
#! Looks up variable location
(define lookup-sto
  (lambda (loc sto total_store)
    (type-case Store sto
      (mtsto () (error 'lookup-sto "undefined id"))
      (aSto (sto_loc sto_value rest_sto)
            (if (equal? loc sto_loc)
                (vxs sto_value total_store)
                (lookup-sto loc rest_sto total_store)
                )
            )
      )
    )
  )


(define lookup-loc
  (lambda (name ds)
    (type-case DefrdSub ds
      (mtsub () (error 'lookup "undefined id"))
      (aSub (bound_name bound_value rest_ds)
            (if (symbol=? name bound_name) ;;issued caused somewhere in here -- returning the locatoin and not the value
                bound_value
                (lookup-loc name rest_ds)
                )
            )
      )
    )
  )


#! Parses and Interprets
(define eval-cfwaes 
  (lambda (cfwaes)
    (eval-interp (parse-cfwaes cfwaes) (mtsub) (mtsto))))

#! Takes intepretation and pulls out numV
(define eval-interp 
  (lambda (cfwaes ds sto)
    (vxs-value (interp-cfwaes cfwaes ds sto))))


;Perry Applicable Test Cases From Project 3
(test (eval-cfwaes '{{fun x {+ 1 3}} 1}) (numV 4))
(test (eval-cfwaes '{with {y 1} {{fun x {+ y x}} 3}}) (numV 4))
(test (eval-cfwaes '{with {y 1} {with {f {fun x {+ y x}}} {f 3}}}) (numV 4))
(test (eval-cfwaes '{with {y 1} {with {f {fun x {+ y x}}} {with {y 100} {f 3}}}}) (numV 4))
(test (eval-cfwaes '{{fun x {+ 1 3}} 1}) (numV 4))

;Perry Test Cases For Project 4
(test (eval-cfwaes '{with {y 0}
                       {with {inc {fun x {+ x 1}}}
                         {seq {seq {assign y {inc y}}
                                   {assign y {inc y}}}
                              {seq {assign y {inc y}}
                                   {assign y {inc y}}}}}}) (numV 4))
(test (eval-cfwaes '{with {y 1}
                       {with {inc {fun x {+ x y}}}
                         {inc 3}}}) (numV 4))
(test (eval-cfwaes '{with {y 1}
                       {with {inc {fun x {+ x y}}}
                         {seq {assign y 2} {inc 3}}}}) (numV 5))
(test (eval-cfwaes '{with {y 1}
                       {with {inc {seq {assign y 2} {fun x {+ x y}}}}
                         {inc 3}}}) (numV 5))
(test (eval-cfwaes '{with {x 3}
                       {seq x {assign x {+ x 1}}}}) (numV 4))
(test (eval-cfwaes '{with {x 3}
                       {seq
                         {assign x {+ x 1}} {assign x {+ x 1}}}}) (numV 5))
(test (eval-cfwaes '{with {x 3}
                       {seq
                        {seq
                         {assign x {+ x 1}} {assign x {+ x 1}}}
                        {assign x {+ x 1}}}}) (numV 6))



;My Test Cases
(test (eval-interp (num 1) (mtsub) (mtsto)) (numV 1)) ;num
(test (eval-interp (binop (op 'add) (num 1) (num 2)) (mtsub) (mtsto)) (numV 3)) ;binop
;;;(test (eval-interp (fun 'x (binop (op 'add) (num 1) (id 'x))) (mtsub) (mtsto)) (closureV 'x (binop (op 'add) (num 1) (id 'x)))) ;fun
(test (eval-interp (if0 (binop (op 'sub) (num 1) (num 1)) (num 3) (num 4)) (mtsub) (mtsto)) (numV 3)) ;if0
(test (eval-interp (app (fun 'x (binop (op 'add) (num 1) (id 'x))) (num 2)) (mtsub) (mtsto)) (numV 3)) ;app
(test (eval-interp (with (with-binding 'x (num 2)) (binop (op 'add) (num 1) (id 'x))) (mtsub) (mtsto)) (numV 3)) ;with
(test (eval-interp (if0 (num 0) (if0 (num 1) (num 3) (num 4)) (num 5)) (mtsub) (mtsto)) (numV 4)) ;nested ifs
(test (eval-interp (app (fun 'y (binop (op 'add) (id 'y) (num 1))) (app (fun 'x (binop (op 'add) (id 'x) (num 1))) (num 1))) (mtsub) (mtsto)) (numV 3)) ;nested app
;errors
(test/exn (eval-interp (id 'x) (mtsub) (mtsto)) "lookup: undefined id") ;id 
(test/exn (eval-interp (binop (op 'add) (num 1) (fun 'x (binop (op 'add) (num 1) (id 'x)))) (mtsub) (mtsto)) "cannot perform arithmetic on functions") ;can't apply arithmetic on function
(test/exn (eval-interp (app (num 1) (num 4)) (mtsub) (mtsto)) "not a function/can't apply to number") ;can't apply to number