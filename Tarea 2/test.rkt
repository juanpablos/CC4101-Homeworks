#lang play

(require "base.rkt")

(test (parser '((deftype nat 
                  (O : nat)
                  (S : (nat -> nat)))
                (O)))
      (Program
       (list
        (DefType 'nat
                 (list
                  (Binding 'O (NoArgType 'nat))
                  (Binding 'S (ArgType '(nat) 'nat)))))
       (App 'O '()))
      )

(test (parser '((deftype bool 
                  (t : bool)
                  (f : bool))
                (fun (x : bool) x)))
      (Program
       (list
        (DefType 'bool
                 (list
                  (Binding 't (NoArgType 'bool))
                  (Binding 'f (NoArgType 'bool)))))
       (Fun (list (Binding 'x (NoArgType 'bool))) (Id 'x)))
      )

(test (parser '((deftype bool
                  (t : bool)
                  (f : bool))
                ((fun (x : bool) x) (t))
                )
              )
      (Program (list (DefType 'bool (list (Binding 't (NoArgType 'bool)) (Binding 'f (NoArgType 'bool))))) (App (Fun (list (Binding 'x (NoArgType 'bool))) (Id 'x)) (list (App 't '()))))
      )

(test (parser '((deftype nat 
                  (O : nat)
                  (S : (nat -> nat)))
                (deftype expr 
                  (num : (nat -> expr))
                  (add : (expr expr -> expr)))   
                (add (num (S (O))) (num (O)))))
      (Program
       (list
        (DefType 'nat
                 (list
                  (Binding 'O (NoArgType 'nat))
                  (Binding 'S (ArgType '(nat) 'nat))))
        (DefType 'expr
                 (list
                  (Binding 'num (ArgType '(nat) 'expr))
                  (Binding 'add (ArgType '(expr expr) 'expr)))))
       (App 'add (list (App 'num (list (App 'S (list (App 'O '()))))) (App 'num (list (App 'O '()))))))
      )

(test (parser '((deftype nat
                  (O : nat)
                  (S : (nat -> nat)))
                (def pred (n : nat) : nat
                  (match n
                    ((case (O) => (O))
                     (case (S n1) => n1))))
                (pred (S (O)))))
      (Program
       (list
        (DefType 'nat
                 (list
                  (Binding 'O (NoArgType 'nat))
                  (Binding 'S (ArgType '(nat) 'nat))))
        (Deff 'pred
              (list
               (Binding 'n (NoArgType 'nat)))
              'nat
              (Match (Id 'n)
                     (list
                      (A-Case (IdPattern 'O) (App 'O '()))
                      (A-Case (TypePattern 'S '(n1)) (Id 'n1))))))
       (App 'pred (list (App 'S (list (App 'O '()))))))
      )

(test (parser '((deftype bool 
                  (t : bool)
                  (f : bool))
                (def not (b : bool) : bool
                  (match b
                    ((case (t) => (f)))))
                (not (f))))
      (Program
       (list
        (DefType 'bool
                 (list
                  (Binding 't (NoArgType 'bool))
                  (Binding 'f (NoArgType 'bool))))
        (Deff 'not
              (list
               (Binding 'b (NoArgType 'bool)))
              'bool
              (Match (Id 'b)
                     (list
                      (A-Case (IdPattern 't) (App 'f '()))))))
       (App 'not (list (App 'f '()))))
      )

(test (parser '((deftype day 
                  (monday : day)
                  (tuesday : day)
                  (wednesday : day)
                  (thursday : day)
                  (friday : day)
                  (saturday : day)
                  (sunday : day))
                (deftype bool
                  (t : bool)
                  (f : bool))
                (def weekday (d : day) : bool
                  (match d
                    ((case (saturday) => (f))
                     (case (sunday) => (f))
                     (case otherday => (t)))))
                (weekday (monday))))
      (Program
       (list
        (DefType 'day
                 (list
                  (Binding 'monday (NoArgType 'day))
                  (Binding 'tuesday (NoArgType 'day))
                  (Binding 'wednesday (NoArgType 'day))
                  (Binding 'thursday (NoArgType 'day))
                  (Binding 'friday (NoArgType 'day))
                  (Binding 'saturday (NoArgType 'day))
                  (Binding 'sunday (NoArgType 'day))))
        (DefType 'bool
                 (list
                  (Binding 't (NoArgType 'bool))
                  (Binding 'f (NoArgType 'bool))))
        (Deff 'weekday
              (list
               (Binding 'd (NoArgType 'day)))
              'bool
              (Match (Id 'd)
                     (list
                      (A-Case (IdPattern 'saturday) (App 'f '()))
                      (A-Case (IdPattern 'sunday) (App 'f '()))
                      (A-Case (Id 'otherday) (App 't '()))))))
       (App 'weekday (list (App 'monday '()))))
      )

(test (parser '((deftype bool 
                  (t : bool)
                  (f : bool))
                (deftype nat 
                  (O : nat)
                  (S : (nat -> nat)))
                (def not (b : bool) : bool
                  (match b
                    ((case (t) => (f))
                     (case (f) => (t)))))
                (def even (n : nat) (b : bool) : bool 
                  (match n
                    ((case (O) => b)
                     (case (S n1) => (even n1 (not b))))))
                (even (S (S (S (O)))) (t))))
      (Program
       (list
        (DefType 'bool
                 (list
                  (Binding 't (NoArgType 'bool))
                  (Binding 'f (NoArgType 'bool))))
        (DefType 'nat
                 (list
                  (Binding 'O (NoArgType 'nat))
                  (Binding 'S (ArgType '(nat) 'nat))))
        (Deff 'not
              (list
               (Binding 'b (NoArgType 'bool)))
              'bool
              (Match (Id 'b)
                     (list
                      (A-Case (IdPattern 't) (App 'f '()))
                      (A-Case (IdPattern 'f) (App 't '())))))
        (Deff 'even
              (list
               (Binding 'n (NoArgType 'nat))
               (Binding 'b (NoArgType 'bool)))
              'bool
              (Match (Id 'n)
                     (list
                      (A-Case (IdPattern 'O) (Id 'b))
                      (A-Case (TypePattern 'S '(n1)) (App 'even (list (Id 'n1) (App 'not (list (Id 'b))))))))))
       (App 'even (list (App 'S (list (App 'S (list (App 'S (list (App 'O '()))))))) (App 't '()))))
      )



;;;;;;;;;;;;;;;;;;;; INTERP TODO: check exceptions

(test (interp (App 'O '()) (extend-env 'O (IndType '() "nat") empty-env))
      (Binding (Binding "O" '()) "nat")
      )

(test (interp (App 'S (list (App 'O '()))) (extend-env 'S (IndType '(nat) "nat") (extend-env 'O (IndType '() "nat") empty-env)))
      (Binding (Binding "S" (list (Binding "O" '()))) "nat")
      )


(test (interp (App 'add (list (App 'num (list (App 'S (list (App 'O '()))))) (App 'num (list (App 'O '())))))
              (extend-env 'add (IndType '(expr expr) "expr") (extend-env 'num (IndType '(nat) "expr") (extend-env 'S (IndType '(nat) "nat") (extend-env 'O (IndType '() "nat") empty-env)))))
      (Binding (Binding "add" (list (Binding "num" (list (Binding "S" (list (Binding "O" '()))))) (Binding "num" (list (Binding "O" '()))))) "expr")
      )

;; identity func
(test (interp (App 'idTy (list (App 'S (list (App 'O '())))))
              (extend-env 'idTy (Function (list (Binding 'n 'nat)) "nat" (Id 'n)) (extend-env 'S (IndType '(nat) "nat") (extend-env 'O (IndType '() "nat") empty-env))))
      (Binding (Binding "S" (list (Binding "O" '()))) "nat")
      )

;; no arg func
(test (interp (App 'no '())
              (extend-env 'no (Function '() "bool" (App 'f '())) (extend-env 'f (IndType '() "bool") (extend-env 't (IndType '() "bool") empty-env))))
      (Binding (Binding "f" '()) "bool")
      )

;; pred
(test (interp (App 'pred (list (App 'S (list (App 'O '())))))
              (extend-env 'pred (Function (list (Binding 'n 'nat)) "nat" (Match (Id 'n) (list (A-Case (IdPattern 'O) (App 'O '())) (A-Case (TypePattern 'S '(n1)) (Id 'n1)))))
                          (extend-env 'S (IndType '(nat) "nat") (extend-env 'O (IndType '() "nat") empty-env))))
      (Binding (Binding "O" '()) "nat")
      )

;; weekday (id as matching clause)
(define week-env (extend-env 'weekday (Function (list (Binding 'd 'day)) "bool"
                                                (Match (Id 'd) (list (A-Case (IdPattern 'saturday) (App 'f '())) (A-Case (IdPattern 'sunday) (App 'f '())) (A-Case (Id 'otherday) (App 't '())))))
                             (extend-env 'f (IndType '() "bool") (extend-env 't (IndType '() "bool") (extend-env 'sunday (IndType '() "day") (extend-env 'saturday (IndType '() "day") (extend-env 'friday (IndType '() "day")
                              (extend-env 'thursday (IndType '() "day") (extend-env 'wednesday (IndType '() "day") (extend-env 'tuesday (IndType '() "day") (extend-env 'monday (IndType '() "day") empty-env))))))))))
  )
           
(test (interp (App 'weekday (list (App 'monday '()))) week-env)
      (Binding (Binding "t" '()) "bool")
      )
(test (interp (App 'weekday (list (App 'sunday '()))) week-env)
      (Binding (Binding "f" '()) "bool")
      )
(test (interp (App 'weekday (list (App 'friday '()))) week-env)
      (Binding (Binding "t" '()) "bool")
      )

;; even (recursion and multiple arguments
(define even-env (extend-env 'even (Function (list (Binding 'n 'nat) (Binding 'b 'bool)) "bool"
                                             (Match (Id 'n) (list (A-Case (IdPattern 'O) (Id 'b)) (A-Case (TypePattern 'S '(n1)) (App 'even (list (Id 'n1) (App 'not (list (Id 'b))))))))) (extend-env 'not
                                              (Function (list (Binding 'b 'bool)) "bool" (Match (Id 'b) (list (A-Case (IdPattern 't) (App 'f '())) (A-Case (IdPattern 'f) (App 't '())))))
                                              (extend-env 'f (IndType '() "bool") (extend-env 't (IndType '() "bool") (extend-env 'S (IndType '(nat) "nat") (extend-env 'O (IndType '() "nat") empty-env))))))
  )

(test (interp (App 'even (list (App 'S (list (App 'S (list (App 'S (list (App 'O '()))))))) (App 't '()))) even-env)
      (Binding (Binding "f" '()) "bool")
      )

(test (interp (App 'even (list (App 'S (list (App 'S (list (App 'O '()))))) (App 't '()))) even-env)
      (Binding (Binding "t" '()) "bool")
      )


;; anonymous function application


(def anony-env (extend-env 'f (IndType '() "bool") (extend-env 't (IndType '() "bool") empty-env)))

(test (interp (App (Fun (list (Binding 'x (NoArgType 'bool))) (Id 'x)) (list (App 't '()))) anony-env)
      (Binding (Binding "t" '()) "bool")
      )

(test (interp (App (Fun (list (Binding 'x (NoArgType 'bool)) (Binding 'y (NoArgType 'bool))) (Id 'y)) (list (App 't '()) (App 'f '()))) anony-env)
      (Binding (Binding "f" '()) "bool")
      )




