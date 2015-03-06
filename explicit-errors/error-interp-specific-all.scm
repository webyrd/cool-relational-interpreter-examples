(load "../mk/mk.scm")

;; error-handling Scheme interpreter
;;
;; two types of error (referencing unbound variable, or taking car/cdr of a non-pair (really a closure))
;;
;; errors are now represented as tagged lists, with specific messages
;;
;; this version of the interpreter uses *every* legal Scheme
;; evaluation order for programs that generate errors (rather than
;; left-to-right order, for example)

(define eval-expo
  (lambda (exp env val)
    (fresh ()
      (absento 'ERROR exp)
      (absento 'ERROR env)
      (absento 'closure exp)
      (conde
        ((== `(quote ,val) exp)
         (not-in-envo 'quote env))
        ((fresh (x body)
           (== `(lambda (,x) ,body) exp)
           (== `(closure ,x ,body ,env) val)
           (symbolo x)))
        ((symbolo exp) (lookupo exp env val))
        ((fresh (e1 e2 v1 v2)
           (== `(cons ,e1 ,e2) exp)
           (conde
             ((absento 'ERROR val)
              (== `(,v1 . ,v2) val)
              (eval-expo e1 env v1)
              (eval-expo e2 env v2))
             ((fresh (msg)
                (== `(ERROR . ,msg) val)
                (conde
                  ((eval-expo e1 env `(ERROR . ,msg)))
                  ((eval-expo e2 env `(ERROR . ,msg)))))))))
        ((fresh (rator rand x body env^ a)
           (== `(,rator ,rand) exp)
           (conde
             ((absento 'ERROR val)
              (eval-expo rator env `(closure ,x ,body ,env^))
              (eval-expo rand env a)
              (eval-expo body `((,x . ,a) . ,env^) val))
             ((fresh (msg)
                (== `(ERROR . ,msg) val)
                (conde
                  (
                   ;; must be careful here!
                   ;;
                   ;; we can't depend on the evaluation of rator to ensure
                   ;; application isn't overlapping with quote, for example
                   (=/= 'quote rator)
                   (=/= 'car rator)
                   (=/= 'cdr rator)
                   (eval-expo rator env `(ERROR . ,msg)))
                  ((=/= 'quote rator)
                   (=/= 'car rator)
                   (=/= 'cdr rator)
                   (eval-expo rand env `(ERROR . ,msg)))
                  ((eval-expo rator env `(closure ,x ,body ,env^))
                   (eval-expo rand env a)
                   (eval-expo body `((,x . ,a) . ,env^) `(ERROR . ,msg)))))))))
        ((fresh (e)
           (== `(car ,e) exp)
           (not-in-envo 'car env)
           (conde
             ((fresh (v1 v2)
                (absento 'ERROR `(,v1 . ,v2))
                (=/= 'closure v1)
                (== v1 val)
                (eval-expo e env `(,v1 . ,v2))))
             ((fresh (msg)
                (== `(ERROR . ,msg) val)
                (conde
                  ((eval-expo e env `(ERROR . ,msg)))
                  ((fresh (v)
                     (== `(ERROR ATTEMPT-TO-TAKE-CAR-OF-NON-PAIR ,v) val)
                     (absento 'ERROR v)
                     (not-pairo v)
                     (eval-expo e env v)))))))))
        ((fresh (e)
           (== `(cdr ,e) exp)
           (not-in-envo 'cdr env)
           (conde
             ((fresh (v1 v2)
                (absento 'ERROR `(,v1 . ,v2))
                (=/= 'closure v1)
                (== v2 val)
                (eval-expo e env `(,v1 . ,v2))))
             ((fresh (msg)
                (== `(ERROR . ,msg) val)
                (conde
                  ((eval-expo e env `(ERROR . ,msg)))
                  ((fresh (v)
                     (== `(ERROR ATTEMPT-TO-TAKE-CDR-OF-NON-PAIR ,v) val)
                     (absento 'ERROR v)
                     (not-pairo v)
                     (eval-expo e env v)))))))))))))


(define (not-in-envo x env)
  (conde
    ((== '() env))
    ((fresh (a d)
       (== `(,a . ,d) env)
       (=/= x a)
       (not-in-envo x d)))))

(define (not-pairo v)
  (fresh (x body env)
    (== `(closure ,x ,body ,env) v)))


(define lookupo
  (lambda (x env t)
    (conde
      ((== env '())
       (== `(ERROR UNBOUND-VARIABLE ,x) t))
      ((fresh (rest y v)
          (== `((,y . ,v) . ,rest) env)
          (conde
            ((== y x) (== v t))
            ((=/= y x) (lookupo x rest t))))))))
