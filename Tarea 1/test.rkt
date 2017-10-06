#lang play
(require "base.rkt")
(print-only-errors #t)

;;;;;; Test enunciado
;; P1
(test (typeof  (num 3) (emptyT-env))
      (list (TNum))
      )
(test (typeof  (add (num 10) (num 3)) (emptyT-env))
      (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)))
      )
(test (typeof  (if0 (num 2) (num 5) (num 3)) (emptyT-env))
      (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)))
      )
(test (typeof  (id 'x) (extendT-env 'x (TNum) (emptyT-env)))
      (list (TNum))
      )
(test (typeof  (add (num 10) (id 'x)) (extendT-env 'x (TVar 1) (emptyT-env)))
      (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TVar 1) (TNum)))
      )
(test (typeof  (app (fun 'x (id 'x)) (num 3)) (emptyT-env))
      (list (TVar 1) (Cnst (TFun (TVar 2) (TVar 2)) (TFun (TNum) (TVar 1))))
      )
(test (typeof  (fun 'x (add (id 'x) (num 1))) (emptyT-env))
      (list (TFun (TVar 1) (TNum)) (Cnst (TVar 1) (TNum)) (Cnst (TNum) (TNum)))
      )
(test (typeof  (fun 'f (fun 'x (app (id 'f) (app (id 'f) (id 'x))))) (emptyT-env))
      (list (TFun (TVar 1) (TFun (TVar 2) (TVar 3))) (Cnst (TVar 1) (TFun (TVar 2) (TVar 4))) (Cnst (TVar 1) (TFun (TVar 4) (TVar 3))))
      )
;; P2
(test (unify (list (Cnst (TFun (TVar 2) (TVar 2)) (TFun (TNum) (TVar 1)))))
      (list (Cnst (TVar 1) (TNum)) (Cnst (TVar 2) (TNum)))
 )
(test (unify (list (Cnst (TVar 1) (TNum)) (Cnst (TNum) (TNum))))
      (list (Cnst (TVar 1) (TNum)))
 )
(test (unify (list (Cnst (TVar 1) (TFun (TVar 2) (TVar 4))) (Cnst (TVar 1) (TFun (TVar 4) (TVar 3)))))
      (list (Cnst (TVar 4) (TVar 3)) (Cnst (TVar 2) (TVar 4)) (Cnst (TVar 1) (TFun (TVar 2) (TVar 4))))
 )
;; P3
(test (runType '(fun (x) (+ x 1)))
      (TFun (TNum) (TNum))
 )
(test (runType '(fun (x) x))
      (TFun (TVar 1) (TVar 1))
 )
(test (runType '(fun (x) 3))
      (TFun (TVar 1) (TNum))
 )
(test/exn (runType 'x) "Exception: free identifier x"
 )
(test/exn (runType '((fun (x) (+ x 1)) (fun (x) x))) "Exception: Type error: cannot unify num with (TFun (TVar 3) (TVar 3))"
 )
(test (runType '(fun (f) (fun (x) (f (f x)))))
      (TFun (TFun (TVar 3) (TVar 3)) (TFun (TVar 3) (TVar 3)))
 )


;;;;;; My tests
;;; typeof
; Test numbers
(test (typeof  (num 3) (extendT-env 'x (TVar 1) (emptyT-env)))
      (list (TNum))
      )
; Test arithmetic
(test (typeof  (add (num 10) (add (num 3) (num 1))) (emptyT-env))
      (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))
            (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)))
      )
(test (typeof  (sub (num 15) (add (num 3) (num 1))) (emptyT-env))
      (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))
            (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)))
      )

(test (typeof  (if0 (num 1) (add (num 3) (num 1)) (sub (num 15) (add (num 3) (num 1)))) (emptyT-env))
      (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))
            (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)))
      )
; Functions
(test (typeof (fun 'x (add (id 'x) (app (fun 'a (add (id 'a) (id 'b))) (sub (num 10) (id 'x))))) (extendT-env 'b (TVar 10) (emptyT-env)))
      (list (TFun (TVar 1) (TNum)) (Cnst (TVar 3) (TNum)) (Cnst (TVar 10) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TVar 1) (TNum))
            (Cnst (TFun (TVar 3) (TNum)) (TFun (TNum) (TVar 2))) (Cnst (TVar 1) (TNum)) (Cnst (TVar 2) (TNum))) ;; ?
      )
; Apps
(test (typeof (app (fun 'x (id 'x)) (add (num 3) (num 5))) (emptyT-env))
      (list (TVar 1) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TFun (TVar 2) (TVar 2)) (TFun (TNum) (TVar 1))))
      )

(test (typeof (app (fun 'x (add (id 'x) (num 4))) (add (num 3) (id 'a))) (extendT-env 'a (TVar 10) (emptyT-env)))
      (list (TVar 1) (Cnst (TVar 2) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TVar 10) (TNum))
            (Cnst (TFun (TVar 2) (TNum)) (TFun (TNum) (TVar 1))))
      )

;;; lookupT-env
(test/exn (lookupT-env 'a (emptyT-env)) "Exception: free identifier a"
          )
(test (lookupT-env 'b (extendT-env 'b (TNum) (emptyT-env)))
          (TNum)
          )
(test (lookupT-env 'b (extendT-env 'b (TVar 1) (extendT-env 'b (TNum) (emptyT-env))))
          (TVar 1)
          )

;;; lookupT-env-app
(test/exn (lookupT-env-app (app (fun 'x (add (num 1) (id 'x))) (num 1)) (emptyT-env)) "Exception: free identifier (app (fun 'x (add (num 1) (id 'x))) (num 1))"
          )
(test (lookupT-env-app (app (fun 'x (add (num 1) (id 'x))) (num 1))
                   (extendT-env (app (fun 'x (add (num 1) (id 'x))) (num 1))(TVar 1) (emptyT-env)))
          (TVar 1)
          )

;;; occurs-in?
(test (occurs-in? (TVar 1) (TFun (TNum) (TVar 1)))
      #t
 )
(test (occurs-in? (TVar 1) (TFun (TNum) (TFun (TVar 1) (TVar 2))))
      #t
 )
(test (occurs-in? (TVar 1) (TNum))
      #f
 )
(test/exn (occurs-in? (TFun (TVar 1) (TNum)) (TVar 1)) "(TFun (TVar 1) num) is not a variable"
 )

;;; equalT?
(test (equalT? (TNum) (TNum))
      #t
 )
(test (equalT? (TNum) (TVar 1))
      #f
 )
(test (equalT? (TVar 1) (TVar 1))
      #t
 )
(test (equalT? (TVar 10) (TFun (TVar 2) (TNum)))
      #f
 )
(test (equalT? (TFun (TNum) (TNum)) (TFun (TVar 1) (TVar 2)))
      #f
 )
(test (equalT? (TFun (TVar 1) (TVar 3)) (TFun (TVar 1) (TVar 2)))
      #f
 )
(test (equalT? (TFun (TVar 1) (TVar 3)) (TFun (TVar 1) (TVar 3)))
      #t
 )
(test (equalT? (TFun (TFun (TVar 1) (TVar 2)) (TVar 3)) (TFun (TFun (TVar 1) (TVar 2)) (TVar 3)))
      #t
 )

;;; substitute
(test (substitute (TVar 1) (TNum) (list (Cnst (TVar 1) (TNum))))
      (list (Cnst (TNum) (TNum)))
 )
(test (substitute (TVar 1) (TFun (TVar 2) (TVar 3)) (list (Cnst (TVar 1) (TNum))))
      (list (Cnst (TFun (TVar 2) (TVar 3)) (TNum)))
 )
(test (substitute (TVar 1) (TNum) (list (Cnst (TVar 1) (TNum)) (Cnst (TVar 2) (TNum)) (Cnst (TVar 10) (TNum)) (Cnst (TVar 2) (TVar 1))))
      (list (Cnst (TNum) (TNum)) (Cnst (TVar 2) (TNum)) (Cnst (TVar 10) (TNum)) (Cnst (TVar 2) (TNum)))
 )
(test (substitute (TVar 1) (TNum) (list (Cnst (TVar 1) (TNum)) (Cnst (TNum) (TVar 1)) (Cnst (TFun (TVar 1) (TVar 2)) (TFun (TVar 1) (TVar 1)))))
      (list (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TFun (TNum) (TVar 2)) (TFun (TNum) (TNum))))
 )
(test (substitute (TVar 1) (TNum) empty)
      empty
 )

;;; unify
(test (unify (list (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))))
      empty
 )
(test (unify (list (Cnst (TVar 1) (TNum)) (Cnst (TVar 1) (TVar 1)) (Cnst (TVar 1) (TNum)) (Cnst (TNum) (TVar 1))))
      (list (Cnst (TVar 1) (TNum)))
 )
(test (unify (list (Cnst (TFun (TVar 1) (TVar 4)) (TFun (TVar 3) (TVar 2))) (Cnst (TVar 1) (TNum))))
      (list (Cnst (TVar 4) (TVar 2)) (Cnst (TVar 3) (TNum)) (Cnst (TVar 1) (TNum)))
 )
(test (unify (list (Cnst (TVar 1) (TVar 2)) (Cnst (TVar 2) (TVar 3)) (Cnst (TVar 3) (TVar 4)) (Cnst (TVar 2) (TNum))))
      (list (Cnst (TVar 4) (TNum)) (Cnst (TVar 3) (TVar 4)) (Cnst (TVar 2) (TVar 3)) (Cnst (TVar 1) (TVar 2)))
 )
(test (unify (list (Cnst (TVar 1) (TNum)) (Cnst (TVar 1) (TVar 2)) (Cnst (TVar 2) (TVar 3)) (Cnst (TVar 3) (TVar 4))))
      (list (Cnst (TVar 4) (TNum)) (Cnst (TVar 3) (TNum)) (Cnst (TVar 2) (TNum)) (Cnst (TVar 1) (TNum)))
 )
(test/exn (unify (list (Cnst (TFun (TVar 1) (TVar 2)) (TVar 10)) (Cnst (TVar 10) (TNum)))) "Exception: Type error: cannot unify (TFun (TVar 1) (TVar 2)) with num"  
 )
(test (unify (list (Cnst (TVar 2) (TFun (TNum) (TVar 3))) (Cnst (TFun (TVar 1) (TVar 4)) (TVar 2))))
      (list (Cnst (TVar 4) (TVar 3)) (Cnst (TVar 1) (TNum)) (Cnst (TVar 2) (TFun (TNum) (TVar 3))))
 )

;;; runType
(test (runType '(+ 2 10))
      (TNum)
 )
(test (runType '(if0 (- 2 10) 8 (+ 5 6)))
      (TNum)
 )
(test (runType '(fun (x) (+ x 10)))
      (TFun (TNum) (TNum))
 )
(test (runType '((fun (x) (+ x 10)) ((fun (x) (- x 8)) 10)))
      (TNum)
 )
(test (runType '(fun (x) (fun (y) (x (y 10)))))
      (TFun (TFun (TVar 4) (TVar 3)) (TFun (TFun (TNum) (TVar 4)) (TVar 3)))
 )
(test (runType '1)
      (TNum)
 )
(test (runType '(with (x (+ 10 5)) (fun (y) (+ x y))))
      (TFun (TNum) (TNum))
 )
(test/exn (runType '(with (x (+ 10 5)) (fun (y) (+ x (+ y z))))) "Exception: free identifier z"
 )
(test (runType '((fun (x) ((fun (y) (+ x (+ y 10))) x)) ((fun (z) (if0 z 10 100)) 500)))
      (TNum)
 )












