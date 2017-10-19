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
  (App cnst args)
  )

;; <case> ::= (case <pattern> => <expr>)
(deftype Case
  (A-Case pattern exp)
  )

;; <pattern> ::= <id>
;;             | (<id> <id>*)
(deftype Pattern
  (IdPattern id)
  (TypePattern type id)
  )

(deftype ArgBind
  (Binding exp type)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            P2. PARSER              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parser :: s-expr -> Prog
(define (parser s-expr)

  (define (parse-types type) ;;parse the types
    (match type
      [(? symbol?) (NoArgType type)] ;; type like (() -> type)
      [(list types ..1 '-> final-type) (ArgType types final-type)] ;; types like (t1+ -> t)
      )
    )
  
  (define (parse-args args) ;;parse the arguments of functions and constructors
    (match args
      [(? empty?) '()]
      [(cons head '()) (parse-args head)] ;; only one argument
      [(list id ': type) (list (Binding id (parse-types type)))] ;; binds the identifier with its type
      [(cons head rest) (append (parse-args head) (parse-args rest))] ;; more than one argument
      )
    )

  (define (parse-defs defs) ;;parse deftype and def
    (match defs
      [(? empty?) empty] ;; no definitions
      [(cons head '()) (parse-defs head)] ;; only one definition
      [(list 'deftype type-id constructors ..1) (list (DefType type-id (parse-args constructors)))]
      [(list 'def fun args ... ': return body) (list (Deff fun (parse-args args) return (parse-exp body)))]
      [(cons head rest) (append (parse-defs head) (parse-defs rest))] ;; more than 1 definition present
      )
    )

  (define (parse-constructors-args args) ;;aux parser for constructors
    (match args
      [(? empty?) '()] ;; no next arg
      [(cons arg1 rest) (append (list (parse-constructors arg1)) (parse-constructors-args rest))] ;; parse the args as other Apps
      )
    )

  (define (parse-constructors type-expression) ;;main function to create constructor applications
    (match type-expression
      [(? symbol?) (Id type-expression)]
      [(cons c '()) (App c '())] ;; constructor with no arg
      [(list (list 'fun fun-args ..1 body) args ..1) (App (Fun (parse-args fun-args) (parse-exp body)) (parse-constructors-args args))]
      [(list t args ..1) (App t (parse-constructors-args args))] ;; constructor with args
      )
    )

  (define (parse-pattern pattern) ;;parse patterns for match expression
    (match pattern
      [(? symbol?) (Id pattern)] ;; an identifier
      [(cons pat '()) (IdPattern pat)] ;; a pattern with only a type
      [(cons pat id) (TypePattern pat id)] ;; a pattern that unfolds identifier
      )
    )

  (define (parse-cases cases) ;;parse cases of a match expression
    (match cases
      [(list 'case pattern '=> exp) (list (A-Case (parse-pattern pattern) (parse-exp exp)))] ;; creates the case
      [(cons one-case '()) (parse-cases one-case)] ;; only one case
      [(cons case rest) (append (parse-cases case) (parse-cases rest))] ;; multiple cases
      )
    )

  (define (parse-exp exp) ;;parse an expression
    (match exp
      [(? symbol?) (Id exp)] ;; just an identifier
      [(list 'fun args ..1 body) (Fun (parse-args args) (parse-exp body))] ;; an anonymous function
      [(list 'match match-exp cases ..1) (Match (parse-exp match-exp) (parse-cases cases))] ;; pattern matching
      [(list type type-args ...) (parse-constructors exp)] ;; types - constructors
      
      )
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

(deftype Env
  (mtEnv)
  (anEnv id val next)
  )

(define empty-env (mtEnv))
(define extend-env anEnv)

(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(anEnv id val rest) (if (symbol=? id x)
                             val
                             (env-lookup x rest))]
    )
  )


(deftype EnvVal
  (Function args return-type body)
  (IndType args return-type)
  )

(deftype Constraint
  (Cnst exp type)
  )

(define (check-types types) ;; refactor
  (match types
    [(? empty?) '()]
    [(Cnst arg exp-type) (def (Binding constructor type) arg)
                         (if (string? exp-type)
                             (if (equal? type exp-type)
                                 constructor
                                 (error "type error, not same type")
                                 )
                             (if (equal? type (symbol->string exp-type))
                                 constructor
                                 (error "type error, not same type")
                                 )
                             )]
    [(cons h t) (append (list (check-types h)) (check-types t))]
    )
  )

(define (resolve-args fun-bindings)
  (match fun-bindings
    [(? empty?) '()]
    [(Binding id type) type]
    [(cons h t) (append (list (resolve-args h)) (resolve-args t))]
    )
  )

(define (resolve-variables fun-bindings)
  (match fun-bindings
    [(? empty?) '()]
    [(Binding id type) id]
    [(cons h t) (append (list (resolve-variables h)) (resolve-variables t))]
    )
  )

(define (create-fun-env variables-arg-pairs the-env)
  (match variables-arg-pairs
    [(? empty?) the-env]
    [(cons h '()) (create-fun-env h the-env)]
    [(Binding var arg) (extend-env var arg the-env)]
    [(cons h t) (def h-env (create-fun-env h the-env))
                (create-fun-env t h-env)]
    )
  )

(define (create-arg-env variables the-env)
  (match variables
    [(cons h '()) (create-arg-env h the-env)]
    [(Binding id type) (extend-env id type the-env)]
    [(cons h t) (def h-env (create-arg-env h the-env))
                (create-arg-env t h-env)]
    )
  )

(define (fold-body body env)
  (interp body env)
  )

(define (interp-func func args env)
  (def (Function the-args return-type the-body) func) ;; the-args can be empty
  
  (def arg-types (resolve-args the-args))
  (def arg-variables (resolve-variables the-args))
  
  (def types-zip (map Cnst args arg-types))
  
  (def types-fin (check-types types-zip))
  (def fun-env (create-fun-env (map Binding arg-variables args) env))

  (def body-result (fold-body the-body fun-env))
  ;(print body-result)

  (if (equal? return-type (Binding-type body-result))
      body-result
      (error "type error, return types are not the same")
      )
  )

(define (interp-type type type-app args)
  (def (IndType the-args return-type) type-app)
  (def types-zip (map Cnst args the-args)) ;; check or raise an error
  (def types-fin (check-types types-zip))
  (Binding (Binding (symbol->string type) types-fin) return-type)
  )

(define (check-match-no id match-id)
  (equal? (symbol->string id) (Binding-exp (Binding-exp match-id)))
  )

(define (check-match-mul id vars match-id)
  (if (equal? (symbol->string id) (Binding-exp (Binding-exp match-id)))
      (equal? (length vars) (length (Binding-type (Binding-exp match-id))))
      #f
      )
  )

(define (bind-types ids env)
  (match ids
    [(cons h '()) (list (bind-types h env))]
    [(Binding id args) (Binding ids (IndType-return-type (env-lookup (string->symbol id) env)))]
    [(cons h t) (append (list (bind-types h env)) (bind-types t env))]
    )
  )

(define (bind-matches vars match-id env)
  (def pre-results (Binding-type (Binding-exp match-id)))
  (def results (bind-types pre-results env))
  (create-fun-env (map Binding vars results) env)
  )


(define (eval-cases match-id cases env)
  (cond
    [(empty? cases) (error "match error, no matching case")]
    [else (def (cons (A-Case pattern return) rest-cases) cases)
          (match pattern
            [(IdPattern id) (if (check-match-no id match-id)
                                (interp return env)
                                (eval-cases match-id rest-cases env)
                                )]
            [(TypePattern id fold) (cond
                                     [(check-match-mul id fold match-id)
                                      (define new-env (bind-matches fold match-id env))
                                      (interp return new-env)]
                                     [else (eval-cases match-id rest-cases env)]
                                     )]
            [(Id n) (interp return env)]
            )]
    )
  )

(define (clean-args args)
  (match args
    [(cons h '()) (list (clean-args h))]
    [(Binding id type) (Binding id (NoArgType-type type))]
    [(cons h t) (append (list (clean-args h)) (clean-args t))]
    )
  )

(define (interp-anonymous fun args env)
  (def (Fun fun-args fun-body) fun)
  (def parsed-fun-args (clean-args fun-args))

  (def fun-arg-types (resolve-args parsed-fun-args))
  (def fun-arg-variables (resolve-variables parsed-fun-args))
  
  (def types-zip (map Cnst args fun-arg-types))
  
  (def types-fin (check-types types-zip))
  
  (def fun-env (create-fun-env (map Binding fun-arg-variables args) env))
  
  (fold-body fun-body fun-env)
  )

     
;interp :: Expr x Env -> Val
(define (interp expr env)
  (match expr
    [(? empty?) '()]
    [(Id n) (env-lookup n env)]
    [(App id  args) (cond
                      [(Fun? id) (interp-anonymous id (interp args env) env)]
                      [else
                       (def application (env-lookup id env)) ;; the env has (Funtion --) (IndType --)
                       (def interp-args (interp args env))
                       (match application
                         [(Function _ _ _) (interp-func application interp-args env)]
                         [(IndType _ _) (interp-type id application interp-args)]
                         )]
                      )]
    [(Match id cases) (def bind-id (interp id env))
                      (eval-cases bind-id cases env)]
    [(cons h l) (append (list (interp h env)) (interp l env))]
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          P4. FUNCIÓN Run           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;run :: s-expr -> String 
(define (run s-expr)
  (void))