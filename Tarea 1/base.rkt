#lang play

;################################ Interprete visto en clases ###########################

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f-name f-arg))


;; parse :: s-expr -> Expr
;; converts s-exprs into Exprs where
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
|#
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n)]
    [ x #:when (symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]    
    [(list 'with (list x e) b) #:when (symbol? x)
         (app (fun x (parse b)) (parse e))]))


;; Abstract Dada Type (ADT) for handling environments 
;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value

;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (env-lookup x rest))]))


;; Values of expressions 
;; <value> ::= (numV <number>)
;;          |  (closureV <sym> <s-expr> <env>) 
(deftype Value
  (numV n)
  (closureV id body env))

;; Auxiliary functions handling numeric Values
(define (op-bin f n1 n2)
  (numV (f (numV-n n1) (numV-n n2))))

(define (op-un f n)
  (numV (f (numV-n n))))


;; eval :: Expr Env -> Value
;; evaluates an expression in a given
;; environment using static scoping 
(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(id x) (env-lookup x env)]
    [(add l r) (op-bin + (eval l env) (eval r env))]
    [(sub l r) (op-bin - (eval l env) (eval r env))]
    [(if0 c t f) (if  (op-un zero? (eval c env))
                      (eval t env)
                      (eval f env))]
    [(app f e) (def (closureV the-arg the-body the-claus-env) (eval f env))
               (def the-ext-env (extend-env the-arg (eval e env) the-claus-env))
               (eval the-body the-ext-env)]))


;; run :: s-expr -> Value
(define (run prog)
  (eval (parse prog) (mtEnv)))





;################################ Definiciones ###########################

(deftype Type
  (TNum)
  (TFun Targ Tret)
  (TVar Symbol))

(deftype Constraint
  (Cnst T1 T2))

(deftype TEnv
  (mtTEnv)
  (anTEnv id Type env))

(define count 0)

(define (get-id)
  (begin
    (set! count (add1 count))
    count))

(define (reset)
  (set! count 0))

(define (prettyfy T)
  (match T
    [(TNum) "num"]
    [(TVar x) (string-append "(TVar " (number->string x) ")")];;????
    [(TFun T1 T2) (string-append "(TFun " (prettyfy T1) " " (prettyfy T2) ")")]))




;################################ Su código va aquí ###########################

;; emptyT-env :: TEnv
;; Wrapper of an empty type environment
(define emptyT-env mtTEnv)


;; extendT-env :: Symbol Type TEnv -> TEnv
;; Wrapper for extending a type environment
(define extendT-env anTEnv)


;; lookupT-env :: Symbol TEnv -> Type
;; Searches the type of an identifier in a type environment (linked list)
(define (lookupT-env x env)
  (match env
    [(mtTEnv) (error 'Exception "free identifier ~a" x)]
    [(anTEnv id Type next) (if (symbol=? id x)
                              Type
                              (lookupT-env x next))]
    )
  )


;; lookupT-env-app :: Expr TEnv -> Type
;; Searches the type of an app expression in the type environment
(define (lookupT-env-app app env)
  (match env
    [(mtTEnv) (error 'Exception "free identifier ~v" app)]
    [(anTEnv id Type next) (if (equal? id app)
                              Type
                              (lookupT-env-app app next))]
    )
  )


;; typeof :: Expr TEnv -> (Type, List[Constraint])
;; Returns the type and the list of constraints of a given expression and a type environment
(define (typeof expr env)
  (reset)

  (define (getType exp a-env app-env)
    (match exp
      [(num _) (list (TNum) a-env app-env)]
      [(add l r) (list (TNum) a-env app-env)]
      [(sub l r) (list (TNum) a-env app-env)]
      [(if0 e tb fb) (def (list the-type the-env the-app-env) (getType tb a-env app-env))
                     (list the-type the-env the-app-env)]
      [(id x) (list (lookupT-env x a-env) a-env app-env)]
      [(fun arg body) (def T (TVar (get-id))) (def new-env (extendT-env arg T a-env))
                      (def (list the-type the-env the-app-env) (getType body new-env app-env))
                      (list (TFun T the-type) the-env the-app-env)]
      [(app fun arg) (def T (TVar (get-id))) (def new-app-env (extendT-env (app fun arg) T app-env))
                     (list T a-env new-app-env)]

      )
    )

  (define (get-cnsts exp a-env app-env)
    (match exp
      [(num _) empty]
      [(add l r) (def (list l-type l-env l-app-env) (getType l a-env app-env)) (def (list r-type r-env r-app-env) (getType r a-env app-env))
                 (append (get-cnsts l l-env l-app-env)
                         (get-cnsts r r-env r-app-env)
                         (list (Cnst l-type (TNum)))
                         (list (Cnst r-type (TNum)))
                         
                         )]
      [(sub l r) (def (list l-type l-env l-app-env) (getType l a-env app-env)) (def (list r-type r-env r-app-env) (getType r a-env app-env))
                 (append (get-cnsts l l-env l-app-env)
                         (get-cnsts r r-env r-app-env)
                         (list (Cnst l-type (TNum)))
                         (list (Cnst r-type (TNum)))
                         
                         )]
      [(if0 e tb fb) (def (list e-type e-env e-app-env) (getType e a-env app-env)) (def (list tb-type tb-env tb-app-env) (getType tb a-env app-env))
                     (def (list fb-type fb-env fb-app-env) (getType fb a-env app-env))
                     (append (get-cnsts e e-env e-app-env)
                             (get-cnsts tb tb-env tb-app-env)
                             (get-cnsts fb fb-env fb-app-env)
                             (list (Cnst e-type (TNum)))
                             (list (Cnst tb-type fb-type))
                             
                             )]
      [(id x) empty]
      [(fun arg body) (get-cnsts body a-env app-env)]
      [(app fun arg) (def (list fun-type fun-env fun-app-env) (getType fun a-env app-env)) (def (list arg-type arg-env arg-app-env) (getType arg a-env app-env))
                     (def app-type (lookupT-env-app (app fun arg) app-env))
                     (append (get-cnsts fun fun-env fun-app-env)
                             (get-cnsts arg arg-env arg-app-env)
                             (list (Cnst fun-type (TFun arg-type app-type)))
                             )]
      )
    )

  (def (list the-type the-env the-app-env) (getType expr env (emptyT-env)))

  (append (list the-type) (get-cnsts expr the-env the-app-env))
  )


;;;;;; P2

;; substitute :: TVAR Type List[Constraint] -> List[Constraint]
;; replaces 'from' TVar type with 'to' in all ocurrences
;; of 'from' in the list of constraints
(define (substitute from to _list)

  (define (recursive-sub to-search change-to a-type)
    (match a-type
      [(TNum) (TNum)]
      [(TVar id) (def (TVar var) to-search)
                 (if (equal? id var)
                     change-to
                     a-type
                     )]
      [(TFun a b) (TFun (recursive-sub to-search change-to a)
                        (recursive-sub to-search change-to b)
                        )]
      )
    )

  (match _list
    [(? empty?) empty]
    [(cons val rest) (def (Cnst t1 t2) val)
                     (def new-t1 (recursive-sub from to t1))
                     (def new-t2 (recursive-sub from to t2))
                     (cons (Cnst new-t1 new-t2)
                           (substitute from to rest))]
    )
  )
                         

;; occurs-in? :: TVAR Type -> Bool
;; checks if a TVar (variable type) occurs as a subexpression of Type
(define (occurs-in? tvar t)
  (if (TVar? tvar)
      (match t
        [(TNum) #f]
        [(TVar v) (def (TVar var) tvar) (equal? v var)]
        [(TFun a b) (or (occurs-in? tvar a) (occurs-in? tvar b))]
        )
      (error 'occurs-in? " ~a is not a variable type TVar" (prettyfy tvar)))
  )


;; equalT? :: Type Type -> Bool
;; checks if 2 expression types are equal, the same
(define (equalT? t1 t2)
  (match t1
    [(TNum) (TNum? t2)]
    [(TVar var) (cond
                  [(TVar? t2) (def (TVar var2) t2) (equal? var var2)]
                  [else #f]
                  )]
    [(TFun a1 b1) (cond
                  [(TFun? t2) (def (TFun a2 b2) t2)
                              (and (equalT? a1 a2) (equalT? b1 b2))]
                  [else #f]
                  )]
    )
  )


;; unify :: List[Constraint] -> List[Constraint]
;; unifies a list of constraints, returning a "simplified" version of the original list
;; remove redundant constraints and unifies obscure dependencies between constraints
(define (unify _list)
  (match _list
    [(? empty?) empty]
    [(cons val rest) (def (Cnst t1 t2) val)
                     (cond
                       [(equalT? t1 t2) (unify rest)]
                       [(and (TVar? t1) (not (occurs-in? t1 t2)))
                        (append (unify (substitute t1 t2 rest))
                                (list (Cnst t1 t2)))]
                       [(and (TVar? t2) (not (occurs-in? t2 t1)))
                        (append (unify (substitute t2 t1 rest))
                                (list (Cnst t2 t1)))]
                       [(and (TFun? t1) (TFun? t2))
                        (def (TFun T1a T1r) t1) (def (TFun T2a T2r) t2)
                        (unify (append rest (list (Cnst T1a T2a) (Cnst T1r T2r))))]
                       [else (error 'Exception "Type error: cannot unify ~a with ~a" (prettyfy t1) (prettyfy t2))]
                       )]
    )
  )


;;;;;; P3

;; runType :: s-expr -> Type
;; returns the type of 's-expr'
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
|#
(define (runType s-expr)
  (def parsed-exp (parse s-expr))
  
  (def (cons the-type the-constraints) (typeof parsed-exp (emptyT-env)))
  (def unified-constraints (unify the-constraints))

  (define (search-relation a-type list-constraints)
    (match list-constraints
      [(? empty?) a-type]
      [(cons val rest) (def (Cnst t1 t2) val)
                       (if (equalT? a-type t1)
                           t2
                           (search-relation a-type rest)
                           )]
      )
    )

  (define (type-planner a-type)
    (match a-type
      [(TNum) (TNum)]
      [(TVar id) (search-relation a-type unified-constraints)]
      [(TFun a b) (TFun (type-planner a) (type-planner b))]
      )
    )
  
  (define (call-until-done a-type)
    (def t (type-planner a-type))
    (if (equalT? a-type t)
        a-type
        (call-until-done t)
        )
    )

  (call-until-done the-type)
  
  )












         