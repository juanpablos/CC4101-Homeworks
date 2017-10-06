#lang play



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      P1. SINTAXIS ABSTRACTA        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; <prog> ::= (<def>* <expr>)
(deftype Prog
  ; complete here
  )

;; <def>  ::= (deftype <id> (<id> : <type>)+)
;;          | (def <id> (<id> : <type>)* : <type> <expr>)
(deftype Def
  ; complete here
  )

;; <type> ::= <id>
;;          | (<type>+ -> <type>)
(deftype Type
  ; complete here
  )

;; <expr> ::= <id>
;;          | (fun (<id> : <type>)+ <expr>)
;;          | (match <expr> (<case>+))
;;          | (<expr> <expr>*)
(deftype Expr
  ; complete here
  )

;; <case> ::= (case <pattern> => <expr>)
(deftype Case
  ; complete here
  )

;; <pattern> ::= <id>
;;             | (<id> <id>*)
(deftype Pattern
  ; complete here
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            P2. PARSER              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parser :: s-expr -> Prog
(define (parser s-expr)
  (void))




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