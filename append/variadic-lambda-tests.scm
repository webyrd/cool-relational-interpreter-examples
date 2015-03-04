(load "interp-with-variadic-lambda.scm")
(load "../test-check.scm")
(load "../matche.scm")

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
;; It doesn't look all that Scheme like, unfortunately.
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
    ((list) '(a b c d e))
    ('(a b c d) (list 'e))
    ('(a b c) (list 'd 'e))
    ('(a b) (list 'c 'd 'e))
    ((list 'a) '(b c d e))
    ('(a) (list 'b 'c 'd 'e))
    (('(a b c d e) ((lambda _.0 _.0)))
     (sym _.0))
    (('() ((lambda _.0 '(a b c d e)))) (=/= ((_.0 quote)))
     (sym _.0))
    (('(a) ((lambda _.0 '(b c d e)))) (=/= ((_.0 quote)))
     (sym _.0))
    (('(a b) ((lambda _.0 '(c d e)))) (=/= ((_.0 quote)))
     (sym _.0))
    (('(a b c) ((lambda _.0 '(d e)))) (=/= ((_.0 quote)))
     (sym _.0))
    (('(a b c d) ((lambda _.0 '(e)))) (=/= ((_.0 quote)))
     (sym _.0))
    (('(a b c d e) ((lambda _.0 '()))) (=/= ((_.0 quote)))
     (sym _.0))))

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
