#lang play



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      P1. SINTAXIS ABSTRACTA        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; <prog> ::= (<def>* <expr>)
(deftype Prog
  (Program defs exp)
  )

;; <def>  ::= (deftype <id> (<id> : <type>)+)
;;          | (def <id> (<id> : <type>)* : <type> <expr>)
(deftype Def
  (DefType type definitions)
  (Deff fun-id args return body)
  )

;; <type> ::= <id>
;;          | (<type>+ -> <type>)
(deftype Type
  (NoArgType type)
  (ArgType types type)
  )

;; <expr> ::= <id>
;;          | (fun (<id> : <type>)+ <expr>)
;;          | (match <expr> (<case>+))
;;          | (<expr> <expr>*)
(deftype Expr
  (Id id)
  (Fun args body)
  (Match exp cases)
  (App fun args)
  )

;; <case> ::= (case <pattern> => <expr>)
(deftype Case
  (A-Case pattern exp)
  )

;; <pattern> ::= <id>
;;             | (<id> <id>*)
(deftype Pattern
  (IdPattern id)
  (TypePattern type)
  )

(deftype ArgBind
  (Binding id type)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            P2. PARSER              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parser :: s-expr -> Prog
(define (parser s-expr)

  (define (parse-types type)
    (match type
      [(? symbol?) (NoArgType type)]
      [(list a-type mt ... '-> final-type) (ArgType (append (list a-type) mt) final-type)]
      )
    )
  
  (define (parse-args args)
    (match args
      [(cons head '()) (parse-args head)]
      [(list id ': type) (Binding id (parse-types type))]
      [(cons head rest) (list (parse-args head) (parse-args rest))]
      )
    )

  (define (parse-defs defs)
    (match defs
      [(? empty?) empty]
      [(cons head '()) (parse-defs head)]
      [(list 'deftype type-id defi md ...) (DefType type-id (parse-args (append (list defi) md)))]
      [(list 'def fun args ... ': return body) (Deff fun (parse-args args) return body)]
      [(cons head rest) (list (parse-defs head) (parse-defs rest))]
      )
    )

  (define (parse-exp exp)
    (void)
    ;; complete
    )
  
  (match s-expr
    [(list defs ... exp) (def the-defs (parse-defs defs))
                         (def the-exp (parse-exp exp))
                         (Program the-defs the-exp)]
    )
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          P3. INTÉRPRETE            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;interp :: Expr x Env -> Val
(define (interp expr env)
  (void))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          P4. FUNCIÓN Run           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;run :: s-expr -> String 
(define (run s-expr)
  (void))