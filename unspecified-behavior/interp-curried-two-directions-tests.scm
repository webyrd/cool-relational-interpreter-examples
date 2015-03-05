(load "../mk/test-check.scm")
(load "interp-curried-two-directions.scm")

;; Find a program that evaluates to different values under
;; left-to-right and right-to-left evaluation:
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

;; Find a program that evaluates to either (you) or (lamp):
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

;; Find a program that evaluates to either (I love you) or (I love lamp):
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



(test "quines-left-to-right"
  (run 1 (q)
    (eval-left-to-righto q q))
  '((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote))
          ((_.0 void)))
     (sym _.0))))

;; doesn't seem to come back
#|
(test "quines-right-to-left"
  (run 1 (q)
    (eval-right-to-lefto q q))
  '((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote))
          ((_.0 void)))
     (sym _.0))))
|#
