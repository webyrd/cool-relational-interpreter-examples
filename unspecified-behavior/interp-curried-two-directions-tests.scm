;; The file 'interp-curried-two-directions.scm' contains *two*
;; relational Scheme interpreters, which differ in their evaluation
;; order for (curried) application and for evaluation order of the
;; arguments to 'cons'.
;;
;; The 'eval-left-to-righto' relation evaluates arguments in
;; left-to-right order, while the 'eval-right-to-lefto' relation
;; evaluates arguments in right-to-left order.  For example, when
;; evaluating the expression (cons e1 e2), where e1 and e2 are
;; sub-expressions, 'eval-left-to-righto' will evaluate e1 before
;; evaluating e2, while 'eval-right-to-lefto' will use the opposite
;; evaluation order.
;;
;; This difference in evaluation order is only visible when an
;; expression contains an effectful expression in argument position.
;; For example, the value of the variable x after evaluating
;; (cons (set! x 5) (set! x 6)) depends on which set! is evaluated
;; second.  The Scheme specification does not specify the order in
;; which expressions in argument position should be evaluated, so
;; the behavior of this example is implementation specific.
;; Vicare Scheme, Petite Chez Scheme, and Racket differ in this
;; regard: Racket evaluates left-to-right, Petite seems to evaluate
;; right-to-left, and Vicare seems to use both left-to-right and
;; right-to-left evaluation, depending on context.
;;
;; The examples in this file show how we can generate programs
;; whose behavior differs under these three implementations.
(load "interp-curried-two-directions.scm")
(load "../mk/test-check.scm")


;; Find a Scheme expression that evaluates to different values
;; under left-to-right and right-to-left evaluation:
(test "1"
  (run 1 (expr v1 v2)
    (=/= v1 v2)
    (eval-left-to-righto expr v1)
    (eval-right-to-lefto expr v2))
  '(((((lambda (_.0)
         (cons _.0 (set! _.0 '_.1)))
       '_.2)
      (_.2 . void)
      (_.1 . void))
     (=/= ((_.0 cons)) ((_.0 quote)) ((_.0 set!)) ((_.0 void)) ((_.1 _.2)))
     (sym _.0)
     (absento (closure _.1) (closure _.2)
              (void _.1) (void _.2)))))

;; The resulting expression, 
;;
;; ((lambda (_.0)
;;    (cons _.0 (set! _.0 '_.1)))
;;  '_.2)
;;
;; evaluates to (_.1 . #<void>) in Vicare and Petite,
;; and evaluates to (_.2 . #<void>) in Racket.



;; Find a Scheme expression that evaluates to either (you) or (lamp):
(test "2"
  (run 1 (expr)
    (eval-left-to-righto expr '(you))
    (eval-right-to-lefto expr '(lamp)))
  '((((lambda (_.0)
        (cons _.0 ((lambda (_.1) '()) (set! _.0 'lamp))))
      'you)
     (=/= ((_.0 cons)) ((_.0 lambda)) ((_.0 quote)) ((_.0 set!)) ((_.0 void))
          ((_.1 quote)) ((_.1 void)))
     (sym _.0 _.1))))

;; The resulting expression,
;;
;; ((lambda (_.0)
;;    (cons _.0 ((lambda (_.1) '()) (set! _.0 'lamp))))
;;  'you)
;;
;; evaluates to (you) in Vicare and Racket,
;; and evaluates to (lamp) in Petite.



;; Find a Scheme expression that evaluates to either (I love you) or (I love lamp):
(test "3"
  (run 1 (expr)
    (eval-left-to-righto expr '(I love you))
    (eval-right-to-lefto expr '(I love lamp)))
  '((((lambda (_.0)
        (_.0 (set! _.0 (lambda (_.1) '(I love lamp)))))
      (lambda (_.2)
        '(I love you)))
     (=/= ((_.0 lambda)) ((_.0 quote)) ((_.0 set!)) ((_.0 void))
          ((_.1 quote)) ((_.1 void)) ((_.2 quote)) ((_.2 void)))
     (sym _.0 _.1 _.2))))

;; The resulting expression,
;;
;; ((lambda (_.0)
;;    (_.0 (set! _.0 (lambda (_.1) '(I love lamp)))))
;;  (lambda (_.2)
;;    '(I love you)))
;;
;; evaluates to (I love you) in Racket,
;; and evaluates to (I love lamp) in Vicare and Petite.


;; The left-to-right interpreter can also be used to generate quines.
(test "quines-left-to-right"
  (run 1 (q)
    (eval-left-to-righto q q))
  '((((lambda (_.0) (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0) (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     (=/= ((_.0 closure)) ((_.0 cons)) ((_.0 quote)) ((_.0 void)))
     (sym _.0))))

;; Generating quines using the right-to-left interpreter seems too inefficient
;; to come back in a reasonable time.  This appears to be due to the awkward
;; way in which the interpreter evaluates '(cons e1 e2)': ideally the
;; interpreter should evaluate e1 first, which might immediately result in failure,
;; and pruning of the search tree.  However, the interpreter evaluates e2 first
;; (at least logically), which may result in cdring down a list an arbitrary distance.
#|
(test "quines-right-to-left"
  (run 1 (q)
    (eval-right-to-lefto q q))
  '((((lambda (_.0) (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0) (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     (=/= ((_.0 closure)) ((_.0 cons)) ((_.0 quote)) ((_.0 void)))
     (sym _.0))))
|#
