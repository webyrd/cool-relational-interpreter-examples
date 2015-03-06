;; The relational Scheme interpreter in
;; 'error-interp-specific-all.scm' produces explicit error messages
;; for two classes of errors, rather than failing when those specific
;; errors are encountered during execution.  The two errors are
;; unbound variable reference, and attempt to take the car/cdr of a
;; non-pair.  Other errors, such as attempts to apply non-procedures,
;; could also be explicitly modelled---currently, such errors result
;; in failure.
;;
;; Error are represented as tagged lists, with specific error
;; messages.  This makes it possible to generate programs that signal
;; specific errors when evaluated in Scheme.
;;
;; This interpreter also tries *every* legal Scheme evaluation order
;; when evaluating arguments to a procedure call/primitive call
;; (rather than left-to-right evaluation order, for example).
(load "../mk/test-check.scm")
(load "error-interp-specific-all.scm")


;; We'll begin with four queries showing the standard behavior
;; of a relational Scheme interpreter, without generating errors.


;; Simple example of the evaluator running "forward", evaluating an
;; expression that doesn't signal an error.
(test "1"
  (run* (q) (eval-expo '(lambda (x) x) '() q))
  '((closure x x ())))

;; Simple example of running "backwards", generating five expressions
;; that evaluate to the identity procedure.
(test "2"
  (run 5 (q) (eval-expo q '() '(closure x x ())))
  '((lambda (x) x)
    (((lambda (_.0) _.0) (lambda (x) x))
     (=/= ((_.0 ERROR)) ((_.0 closure)))
     (sym _.0))
    ((car (cons (lambda (x) x) '_.0))
     (absento (ERROR _.0) (closure _.0)))
    ((cdr (cons '_.0 (lambda (x) x)))
     (absento (ERROR _.0) (closure _.0)))
    ((car (cons (lambda (x) x) (lambda (_.0) _.1)))
     (=/= ((_.0 ERROR)) ((_.0 closure)))
     (sym _.0)
     (absento (ERROR _.1) (closure _.1)))))

;; Obligatory quine generation.  We generate a program 'q' that
;; evaluates to itself.
(test "3"
  (run 1 (q)
    (eval-expo q '() q))
  '((((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     (=/= ((_.0 ERROR)) ((_.0 closure))) (sym _.0))))

;; Similarly, we can generate Scheme expressions that evaluate to the
;; list (I love you).
(test "4"
  (run 10 (q)
    (eval-expo q '() '(I love you)))
  '('(I love you)
    (cons 'I '(love you))
    ((car '((I love you) . _.0))
     (absento (ERROR _.0) (closure _.0)))
    ((cdr '(_.0 I love you))
     (absento (ERROR _.0) (closure _.0)))
    (((lambda (_.0) '(I love you)) '_.1)
     (=/= ((_.0 ERROR)) ((_.0 closure)))
     (sym _.0)
     (absento (ERROR _.1) (closure _.1)))
    (cons 'I (cons 'love '(you)))
    (((lambda (_.0) _.0) '(I love you))
     (=/= ((_.0 ERROR)) ((_.0 closure)))
     (sym _.0))
    (((lambda (_.0) '(I love you)) (lambda (_.1) _.2))
     (=/= ((_.0 ERROR)) ((_.0 closure)) ((_.1 ERROR)) ((_.1 closure)))
     (sym _.0 _.1)
     (absento (ERROR _.2) (closure _.2)))
    ((cons (car '(I . _.0)) '(love you))
     (absento (ERROR _.0) (closure _.0)))
    ((cons (cdr '(_.0 . I)) '(love you))
     (absento (ERROR _.0) (closure _.0)))))


;; Now let's evaluate (and generate!) Scheme expressions that signal
;; an error.


;; Evaluating (car (lambda (x) x)) produces an error, represented as a
;; tagged list.  Separating the error description into a generic tag
;; (ERROR), a specific error-type tag
;; (ATTEMPT-TO-TAKE-CAR-OF-NON-PAIR), and an "irritant" value
;; ((closure x x ())) gives us useful information when running
;; forward, and gives us lots of control over the query when running
;; backwards.
(test "5"
  (run* (q) (eval-expo '(car (lambda (x) x)) '() q))
  '((ERROR ATTEMPT-TO-TAKE-CAR-OF-NON-PAIR (closure x x ()))))

;; Taking the cdr rather than the car of a procedure gives us a
;; different error-type tag.
(test "6"
  (run* (q) (eval-expo '(cdr (lambda (x) x)) '() q))
  '((ERROR ATTEMPT-TO-TAKE-CDR-OF-NON-PAIR (closure x x ()))))

;; Of course, evaluation works inside-out...
(test "7"
  (run* (q) (eval-expo '(car (cdr (lambda (x) x))) '() q))
  '((ERROR ATTEMPT-TO-TAKE-CDR-OF-NON-PAIR (closure x x ()))))

;; Another type of error: unbound variable reference, with irritant
;; 'x'.
(test "8"
  (run* (q)
    (eval-expo `((lambda (y) x) (lambda (z) z)) '() q))
  '((ERROR UNBOUND-VARIABLE x)))

;; Time to run backwards!  Let's generate ten Scheme expressions that
;; evaluate to one of the error types we explicitly model.  Due to the
;; order of the 'conde' clauses in 'eval-expo', all ten of these
;; expressions generate UNBOUND-VARIABLE errors.
(test "9"
  (run 10 (q msg)
    (eval-expo q '() `(ERROR . ,msg)))
  '(((_.0 (UNBOUND-VARIABLE _.0))
     (=/= ((_.0 ERROR)) ((_.0 closure))) (sym _.0))
    (((cons _.0 _.1) (UNBOUND-VARIABLE _.0))
     (=/= ((_.0 ERROR)) ((_.0 closure))) (sym _.0)
     (absento (ERROR _.1) (closure _.1)))
    (((cons _.0 _.1) (UNBOUND-VARIABLE _.1))
     (=/= ((_.1 ERROR)) ((_.1 closure))) (sym _.1)
     (absento (ERROR _.0) (closure _.0)))
    (((_.0 _.1) (UNBOUND-VARIABLE _.0))
     (=/= ((_.0 ERROR)) ((_.0 car)) ((_.0 cdr))
          ((_.0 closure)) ((_.0 quote)))
     (sym _.0) (absento (ERROR _.1) (closure _.1)))
    (((_.0 _.1) (UNBOUND-VARIABLE _.1))
     (=/= ((_.0 car)) ((_.0 cdr)) ((_.0 quote)) ((_.1 ERROR))
          ((_.1 closure)))
     (sym _.1) (absento (ERROR _.0) (closure _.0)))
    (((car _.0) (UNBOUND-VARIABLE _.0))
     (=/= ((_.0 ERROR)) ((_.0 closure))) (sym _.0))
    (((cdr _.0) (UNBOUND-VARIABLE _.0))
     (=/= ((_.0 ERROR)) ((_.0 closure))) (sym _.0))
    (((cons (cons _.0 _.1) _.2) (UNBOUND-VARIABLE _.0))
     (=/= ((_.0 ERROR)) ((_.0 closure))) (sym _.0)
     (absento (ERROR _.1) (ERROR _.2) (closure _.1)
              (closure _.2)))
    (((cons _.0 (cons _.1 _.2)) (UNBOUND-VARIABLE _.1))
     (=/= ((_.1 ERROR)) ((_.1 closure))) (sym _.1)
     (absento (ERROR _.0) (ERROR _.2) (closure _.0)
              (closure _.2)))
    (((cons (cons _.0 _.1) _.2) (UNBOUND-VARIABLE _.1))
     (=/= ((_.1 ERROR)) ((_.1 closure))) (sym _.1)
     (absento (ERROR _.0) (ERROR _.2) (closure _.0)
              (closure _.2)))))

;; Running backwards again, this time specifying that the generated
;; Scheme expressions must signal an ATTEMPT-TO-TAKE-CAR-OF-NON-PAIR
;; error when evaluated.  We leave the irritant unspecified.
(test "10"
  (run 5 (q val)
    (eval-expo q '() `(ERROR ATTEMPT-TO-TAKE-CAR-OF-NON-PAIR ,val)))
  '((((car (lambda (_.0) _.1))
      (closure _.0 _.1 ()))
     (=/= ((_.0 ERROR)) ((_.0 closure)))
     (sym _.0)
     (absento (ERROR _.1) (closure _.1)))
    (((cons (car (lambda (_.0) _.1)) _.2)
      (closure _.0 _.1 ()))
     (=/= ((_.0 ERROR)) ((_.0 closure)))
     (sym _.0)
     (absento (ERROR _.1) (ERROR _.2) (closure _.1) (closure _.2)))
    (((cons _.0 (car (lambda (_.1) _.2)))
      (closure _.1 _.2 ()))
     (=/= ((_.1 ERROR)) ((_.1 closure))) (sym _.1)
     (absento (ERROR _.0) (ERROR _.2) (closure _.0) (closure _.2)))
    ((((car (lambda (_.0) _.1)) _.2)
      (closure _.0 _.1 ()))
     (=/= ((_.0 ERROR)) ((_.0 closure)))
     (sym _.0)
     (absento (ERROR _.1) (ERROR _.2) (closure _.1) (closure _.2)))
    (((cdr (car (lambda (_.0) _.1)))
      (closure _.0 _.1 ()))
     (=/= ((_.0 ERROR)) ((_.0 closure)))
     (sym _.0)
     (absento (ERROR _.1) (closure _.1)))))

;; Running backwards, specifying that the generated Scheme expressions
;; must signal an UNBOUND-VARIABLE error when evaluated.  We futher
;; specify that the irritant must be the (unbound) variable 'foo'.
(test "11"
  (run 10 (q)
    (eval-expo q '() `(ERROR UNBOUND-VARIABLE foo)))
  '(foo
    ((cons foo _.0)
     (absento (ERROR _.0) (closure _.0)))
    ((cons _.0 foo)
     (absento (ERROR _.0) (closure _.0)))
    ((foo _.0)
     (absento (ERROR _.0) (closure _.0)))
    ((_.0 foo) (=/= ((_.0 car)) ((_.0 cdr)) ((_.0 quote)))
     (absento (ERROR _.0) (closure _.0)))
    (car foo)
    (cdr foo)
    ((cons (cons foo _.0) _.1)
     (absento (ERROR _.0) (ERROR _.1)
              (closure _.0) (closure _.1)))
    ((cons _.0 (cons foo _.1))
     (absento (ERROR _.0) (ERROR _.1)
              (closure _.0) (closure _.1)))
    ((cons (cons _.0 foo) _.1)
     (absento (ERROR _.0) (ERROR _.1)
              (closure _.0) (closure _.1)))))

;; Running forward, demonstrating that the interpreter tries all
;; evaluation orders, and therefore can signal an UNBOUND-VARIABLE
;; error for any of the four unbound variables.
(test "12"
  (run* (q)
    (eval-expo '((w x) (y z)) '() q))
  '((ERROR UNBOUND-VARIABLE w)
    (ERROR UNBOUND-VARIABLE x)
    (ERROR UNBOUND-VARIABLE y)
    (ERROR UNBOUND-VARIABLE z)))
