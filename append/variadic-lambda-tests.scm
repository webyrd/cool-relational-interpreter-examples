;; This version of the relational interpreter in
;; 'interp-with-variadic-lambda.scm' supports 'apply', variadic
;; 'lambda'/application, multi-argument 'lambda'/application, and a
;; fair number of built-ins, such as 'quote', 'list', and 'cons'.
;;
;; Importantly, 'apply' has been moved towards the top of the 'conde'
;; in 'eval-expo', ensuring that the answers will contain many uses of
;; 'apply'.  In general, to get more answers containing a form or
;; primitive function, move the form towards the top of the 'conde' in
;; 'eval-expo' (and vice versa to de-emphasize a form).  The ordering
;; of the 'conde' clauses give us some crude control over how
;; miniKanren explores the search space of terms.
(load "interp-with-variadic-lambda.scm")
(load "../test-check.scm")
(load "../matche.scm")

;; Helper Scheme predicate for testing
(define member? (lambda (x ls) (not (not (member x ls)))))

;; Standard Scheme definition of append.  I've wrapped the definition
;; in a 'let' to avoid shadowing Scheme's built-in 'append'
;; definition.
(let ()
  
  (define append
    (lambda (l s)
      (cond
        ((null? l) s)
        (else (cons (car l) (append (cdr l) s))))))

  (test "Scheme append-1"
    (append '(a b c) '(d e))
    '(a b c d e))
  
  )


;; Our normal relational 'appendo' definition, written in miniKanren.
;; 'appendo' doesn't look very Scheme-like, unfortunately.
(let ()

  (define appendo
    (lambda (l s out)
      (conde
        ((== '() l) (== s out))
        ((fresh (a d res)
           (== `(,a . ,d) l)
           (== `(,a . ,res) out)
           (appendo d s res))))))

  (test "appendo-1"
    (run* (q) (appendo '(a b c) '(d e) q))
    '((a b c d e)))

  (test "appendo-2"
    (run* (q) (appendo '(a b c) q '(a b c d e)))
    '((d e)))

  (test "appendo-3"
    (run* (x y) (appendo x y '(a b c d e)))
    '((() (a b c d e))
      ((a) (b c d e))
      ((a b) (c d e))
      ((a b c) (d e))
      ((a b c d) (e))
      ((a b c d e) ())))

  (test "appendo-4"
    (run 5 (x y z) (appendo x y z))
    '((() _.0 _.0)
      ((_.0) _.1 (_.0 . _.1))
      ((_.0 _.1) _.2 (_.0 _.1 . _.2))
      ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
      ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))))
  
  )

;; Even the pattern-matching version of 'appendo' doesn't look that
;; much like the Scheme code.
(let ()

  (define appendo
    (lambda (l s out)
      (matche (l s out)
        ((() ,s ,s))
        (((,a . ,d) ,s (,a . ,res)) (appendo d s res)))))

  (test "appendo-1"
    (run* (q) (appendo '(a b c) '(d e) q))
    '((a b c d e)))

  (test "appendo-2"
    (run* (q) (appendo '(a b c) q '(a b c d e)))
    '((d e)))

  (test "appendo-3"
    (run* (x y) (appendo x y '(a b c d e)))
    '((() (a b c d e))
      ((a) (b c d e))
      ((a b) (c d e))
      ((a b c) (d e))
      ((a b c d) (e))
      ((a b c d e) ())))

  (test "appendo-4"
    (run 5 (x y z) (appendo x y z))
    '((() _.0 _.0)
      ((_.0) _.1 (_.0 . _.1))
      ((_.0 _.1) _.2 (_.0 _.1 . _.2))
      ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
      ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))))
  
  )



;; With the relational Scheme interpreter written in miniKanren, we
;; can write the *Scheme* definition of 'append', and treat that
;; *function* as a *relation*.  This is because the interpreter itself
;; is a relation.
;;
;; Running append "forwards":
(test "Scheme-append-under-relational-interpreter-1"
  (run* (q)
    (evalo
     '(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     q))
  '((a b c d e)))

;; Running append "backwards:"
(test "Scheme-append-under-relational-interpreter-2"
  (run 6 (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x ,y))
     '(a b c d e)))
  '(('() '(a b c d e))
    ('(a) '(b c d e))
    ('(a b) '(c d e))
    ('(a b c) '(d e))
    ('(a b c d) '(e))
    ('(a b c d e) '())))

;; Replacing 'run 6' with 'run*' in
;; Scheme-append-under-relational-interpreter-2 results in divergence
;; (looping forever).  This seems bad.  Aren't there only 6 answers?

;; Let's try to generate a seventh answer:
(test "Scheme-append-under-relational-interpreter-3"
  (run 7 (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x ,y))
     '(a b c d e)))
  '(('() '(a b c d e))
    ('(a) '(b c d e))
    ('(a b) '(c d e))
    ('(a b c) '(d e))
    ('(a b c d) '(e))
    ('(a b c d e) '())
    ('(a b c d e) (list))))

;; Whoa!  The last answer has a call to 'list' with no arguments,
;; producing the empty list!  Because we are running 'append' in the
;; context of the relational Scheme interpreter, the logic variables
;; 'x' and 'y' in the body of the 'letrec' represent *arbitrary Scheme
;; expressions* that evaluate to lists of symbols.

;; Let's look at a few more answers:
(test "Scheme-append-under-relational-interpreter-4"
  (run 20 (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x ,y))
     '(a b c d e)))
  '(('() '(a b c d e))
    ('(a) '(b c d e))
    ('(a b) '(c d e))
    ('(a b c) '(d e))
    ('(a b c d) '(e))
    ('(a b c d e) '())
    ('(a b c d e) (list))
    (('() (apply (lambda _.0 '(a b c d e)) '()))
     (=/= ((_.0 quote)))
     (sym _.0))
    (('(a b c d e) (apply (lambda _.0 _.0) '()))
     (sym _.0))
    (('(a) (apply (lambda _.0 '(b c d e)) '()))
     (=/= ((_.0 quote)))
     (sym _.0))
    (('(a b) (apply (lambda _.0 '(c d e)) '()))
     (=/= ((_.0 quote)))
     (sym _.0))
    (('(a b c) (apply (lambda _.0 '(d e)) '()))
     (=/= ((_.0 quote)))
     (sym _.0))
    (('(a b c d) (apply (lambda _.0 '(e)) '()))
     (=/= ((_.0 quote)))
     (sym _.0))
    (('(a b c d e) (apply (lambda _.0 '()) '()))
     (=/= ((_.0 quote)))
     (sym _.0))
    ('(a b c d) (list 'e))
    (('(a b c d) (apply (lambda _.0 _.0) '(e)))
     (sym _.0))
    (('() (apply (lambda _.0 '(a b c d e)) '(_.1)))
     (=/= ((_.0 quote)))
     (sym _.0)
     (absento (closure _.1)))
    (('(a) (apply (lambda _.0 '(b c d e)) '(_.1)))
     (=/= ((_.0 quote)))
     (sym _.0)
     (absento (closure _.1)))
    (('(a b) (apply (lambda _.0 '(c d e)) '(_.1)))
     (=/= ((_.0 quote)))
     (sym _.0)
     (absento (closure _.1)))
    (('(a b c) (apply (lambda _.0 '(d e)) '(_.1)))
     (=/= ((_.0 quote)))
     (sym _.0)
     (absento (closure _.1)))))

;; Sure enough, later answers call 'list', and even use variadic
;; 'lambda' and procedure application.  So our Scheme 'append',
;; running in the relational interpreter, is more general than
;; 'appendo'!

;; We can recapture the behavior of 'appendo', in which we restrict
;; the arguments to lists of values (rather than expressions that
;; *evaluate* to lists of values) by a careful use of 'quote' inside
;; the body of the 'letrec':
(test "Scheme-append-under-relational-interpreter-5"
  (run* (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (quote ,x) (quote ,y)))
     '(a b c d e)))
  '((() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())))


;; In addition to inferring the two list arguments in an 'append'
;; call, we can infer the actual use of 'append' in the call!

;; Our first attempt to infer the use of 'append' is unsuccessful.
;; miniKanren "cheats" by generating a variadic lambda expression
;; whose body returns the "output" list.
(test "infer-append-use-1"
  (run 1 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
  '(((lambda _.0 '(a b c d e)) (=/= ((_.0 quote))) (sym _.0))))

;; We can use the 'absento' constraint to keep miniKanren from
;; cheating.  The constraint '(absento 'a q)' ensures that the symbol
;; 'a'---which occurs in both the input to the call and the output---
;; does not occur in the expression we are trying to infer.
;;
;; This results in the expected answer, 'append', and a second
;; expression that also evaluates to the append procedure.
(test "infer-append-use-2"
  (run 2 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e))
    (absento 'a q))
  '(append
    ((apply (lambda _.0 append) '())
     (=/= ((_.0 a)) ((_.0 append)))
     (sym _.0))))


(test "infer-car-1"
  (run 2 (q)
    (absento 'a q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons ,q (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
  '((car l)
    ((apply (lambda _.0 (car l)) s)
     (=/= ((_.0 a)) ((_.0 car)) ((_.0 l))) (sym _.0))))




(define I-love-you-append (run 1000 (q)
                            (evalo
                             `(letrec ((append (lambda (l s)
                                                 (if (null? l)
                                                     s
                                                     (cons (car l) (append (cdr l) s))))))
                                ,q)
                             '(I love you))))

;; a few interesting answers...

(test "I-love-you-append-1"
  (member? '(apply append '((I love) (you)))
           I-love-you-append)
  #t)

(test "I-love-you-append-2"
  (member? '((apply (lambda _.0 (apply append '((I love) (you)))) '())
             (=/= ((_.0 append)) ((_.0 apply)) ((_.0 quote)))
             (sym _.0))
           I-love-you-append)
  #t)

(test "I-love-you-append-3"
  (member? '(((lambda _.0 '(I love you)) append append append append)
             (=/= ((_.0 quote))) (sym _.0))
           I-love-you-append)
  #t)

(test "I-love-you-append-4"
  (member? '((apply (lambda _.0 (apply append '((I) (love you)))) '())
             (=/= ((_.0 append)) ((_.0 apply)) ((_.0 quote)))
             (sym _.0))
           I-love-you-append)
  #t)

(test "I-love-you-append-5"
  (member? '((apply append (apply (lambda _.0 '((I love) (you))) '()))
             (=/= ((_.0 quote))) (sym _.0))
           I-love-you-append)
  #t)

(test "I-love-you-append-6"
  (member? '((apply (lambda _.0 (car _.0)) '((I love you)))
             (=/= ((_.0 car))) (sym _.0))
           I-love-you-append)
  #t)

(test "I-love-you-append-7"
  (member? '(((lambda _.0 '(I love you)) append append append append)
             (=/= ((_.0 quote))) (sym _.0))
           I-love-you-append)
  #t)


(test "simple quines"
  (run 5 (q) (evalo q q))
  '(#t
    #f
    ((apply
      (lambda _.0
        (list 'apply (apply (lambda (_.1) _.1) _.0)
              (list 'quote _.0)))
      '((lambda _.0
          (list 'apply (apply (lambda (_.1) _.1) _.0)
                (list 'quote _.0)))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1))
    ((apply
      (lambda _.0
        (list (apply (lambda _.1 'apply) '())
              (apply (lambda (_.2) _.2) _.0) (list 'quote _.0)))
      '((lambda _.0
          (list (apply (lambda _.1 'apply) '())
                (apply (lambda (_.2) _.2) _.0) (list 'quote _.0)))))
     (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 quote)) ((_.1 closure))
          ((_.1 quote)) ((_.2 closure)))
     (sym _.0 _.1 _.2))
    ((apply (lambda _.0 (list 'apply _.0 (list 'quote _.0)))
            '(lambda _.0 (list 'apply _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))))



(define quines-in-context-of-append
  (run 60 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        ,q)
     q)))


(test "quines-in-context-of-append-1"
  (member? '((apply (lambda _.0 (list 'apply _.0 (list 'quote _.0)))
                    '(lambda _.0 (list 'apply _.0 (list 'quote _.0))))
             (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
             (sym _.0))
           quines-in-context-of-append)
  #t)

(test "quines-in-context-of-append-2"
  (member? '((apply
              (lambda _.0
                (list 'apply (apply append _.0) (list 'quote _.0)))
              '(()
                (lambda _.0
                  (list 'apply (apply append _.0) (list 'quote _.0)))))
             (=/= ((_.0 append)) ((_.0 apply)) ((_.0 closure))
                  ((_.0 list)) ((_.0 quote)))
             (sym _.0))
           quines-in-context-of-append)
  #t)

(test "quines-in-context-of-append-3"
  (member? '((apply
              (lambda _.0
                (list 'apply (apply append _.0)
                      ((lambda _.1 _.1) 'quote _.0)))
              '(()
                (lambda _.0
                  (list 'apply (apply append _.0)
                        ((lambda _.1 _.1) 'quote _.0)))))
             (=/= ((_.0 append)) ((_.0 apply)) ((_.0 closure))
                  ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
                  ((_.1 closure)))
             (sym _.0 _.1))
           quines-in-context-of-append)
  #t)

(test "quines-in-context-of-append-4"
  (member? '((apply
              (lambda _.0
                (list 'apply (apply append _.0)
                      (apply (lambda _.1 (list 'quote _.1)) _.0)))
              '(()
                (lambda _.0
                  (list 'apply (apply append _.0)
                        (apply (lambda _.1 (list 'quote _.1)) _.0)))))
             (=/= ((_.0 append)) ((_.0 apply)) ((_.0 closure))
                  ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
                  ((_.1 closure)) ((_.1 list)) ((_.1 quote)))
             (sym _.0 _.1))
           quines-in-context-of-append)
  #t)

(test "quines-in-context-of-append-5"
  (member? '((apply
              (lambda _.0
                (list (apply (lambda _.1 'apply) _.0)
                      (apply append _.0) (list 'quote _.0)))
              '(()
                (lambda _.0
                  (list (apply (lambda _.1 'apply) _.0)
                        (apply append _.0) (list 'quote _.0)))))
             (=/= ((_.0 append)) ((_.0 apply)) ((_.0 closure))
                  ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
                  ((_.1 closure)) ((_.1 quote)))
             (sym _.0 _.1))
           quines-in-context-of-append)
  #t)

(test "quines-in-context-of-append-6"
  (member? '((apply
              (lambda _.0
                (list 'apply (apply append _.0)
                      (apply (lambda _.1 (list 'quote _.0)) _.0)))
              '(()
                (lambda _.0
                  (list 'apply (apply append _.0)
                        (apply (lambda _.1 (list 'quote _.0)) _.0)))))
             (=/= ((_.0 _.1)) ((_.0 append)) ((_.0 apply))
                  ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
                  ((_.0 quote)) ((_.1 closure)) ((_.1 list))
                  ((_.1 quote)))
             (sym _.0 _.1))
           quines-in-context-of-append)
  #t)

(test "quines-in-context-of-append-7"
  (member? '((apply
              (lambda _.0
                (list 'apply (apply append _.0)
                      (list 'quote (apply (lambda _.1 _.1) _.0))))
              '(()
                (lambda _.0
                  (list 'apply (apply append _.0)
                        (list 'quote (apply (lambda _.1 _.1) _.0))))))
             (=/= ((_.0 append)) ((_.0 apply)) ((_.0 closure))
                  ((_.0 lambda)) ((_.0 list)) ((_.0 quote))
                  ((_.1 closure)))
             (sym _.0 _.1))
           quines-in-context-of-append)
  #t)

(test "quines-in-context-of-append-8"
  (member? '((apply
              (lambda _.0
                (list 'apply (apply (lambda (_.1) _.1) _.0)
                      (apply (lambda _.2 ((lambda _.3 _.3) 'quote _.2))
                             _.0)))
              '((lambda _.0
                  (list 'apply (apply (lambda (_.1) _.1) _.0)
                        (apply (lambda _.2 ((lambda _.3 _.3) 'quote _.2))
                               _.0)))))
             (=/= ((_.0 apply)) ((_.0 closure)) ((_.0 lambda))
                  ((_.0 list)) ((_.0 quote)) ((_.1 closure))
                  ((_.2 closure)) ((_.2 lambda)) ((_.2 quote))
                  ((_.3 closure)))
             (sym _.0 _.1 _.2 _.3))
           quines-in-context-of-append)
  #t)


