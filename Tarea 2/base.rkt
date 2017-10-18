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
  (Binding id type)
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

(define (interp-func func args)
  (def (Function the-args return-type the-body) func)
  (void)
  )

(define (check-types types)
  (match types
    [(? empty?) ""]
    [(Cnst arg exp-type) (def (Binding constructor type) arg)
                     (if (equal? type (symbol->string exp-type))
                         constructor
                         (error "error found, not same type")
                         )]
    [(cons h t) (string-append " " (check-types h) (check-types t))]
    )
  )

(define (interp-type type type-app args)
  (def (IndType the-args return-type) type-app)
  (def types-zip (map Cnst args the-args)) ;; check or raise an error
  (def types-fin (check-types types-zip))
  (Binding (string-append "(" (symbol->string type) types-fin ")") return-type)
  )
  
         

;interp :: Expr x Env -> Val
(define (interp expr env)
  (match expr
    [(? empty?) '()]
    [(App id  args) (def application (env-lookup id env)) ;; the env has (Funtion --) (IndType --)
                    (def interp-args (interp args env))
                    (match application
                      [(Function _ _ _) (interp-func application interp-args)]
                      [(IndType _ _) (interp-type id application interp-args)]
                      )]
    [(cons h l) (append (list (interp h env)) (interp l env))]
    )
  )
         


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          P4. FUNCIÓN Run           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;run :: s-expr -> String 
(define (run s-expr)
  (void))