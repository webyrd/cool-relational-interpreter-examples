(load "../mk/mk.scm")

;;; This version of the code uses curried lambda/application,
;;; uses 'cons' rather than 'list', and contains two interpreters:
;;; one left-to-right, and one right-to-left.


(define printg
  (lambda (format-str . args)
    (lambda (c)
      (let ((args (walk* args (c->S c))))
        (apply printf format-str args))
      (newline)
      c)))


;;; non-directional
(define-syntax extend-env
  (syntax-rules ()
    [(_ x addr env)
     `(ext-env ,x ,addr ,env)]))

(define-syntax extend-store
  (syntax-rules ()
    [(_ addr val store)
     `(ext-store ,addr ,val ,store)]))

(define lookup-varo
  (lambda (x env addr)
    (fresh (y a rest)
      (== `(ext-env ,y ,a ,rest) env)
      (conde
        ((== y x) (== a addr))
        ((=/= y x) (lookup-varo x rest addr))))))

(define lookup-addro
  (lambda (addr store val)
    (fresh (a v rest)
      (== `(ext-store ,a ,v ,rest) store)
      (conde
        ((== a addr) (== v val))
        ((=/= a addr) (lookup-addro addr rest val))))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((== '() env))
      ((fresh (y v rest)
         (== `(ext-env ,y ,v ,rest) env)
         (=/= y x)
         (not-in-envo x rest))))))








(define eval-left-to-righto
  (lambda (exp val)
    (fresh (store-out addr-out)
      (eval-left-to-right-expo exp '() '() 'z store-out addr-out val)
      (absento 'void exp))))

(define eval-left-to-right-expo
  (lambda (exp env store-in addr-in store-out addr-out val)
      (conde

        ((== `(quote ,val) exp)
         (== store-in store-out)
         (== addr-in addr-out)
         (not-in-envo 'quote env)
         (absento 'closure val))
        
        ((fresh (addr)
           (symbolo exp)
           (== store-in store-out)
           (== addr-in addr-out)
           (lookup-varo exp env addr)
           (lookup-addro addr store-in val)))

        ((fresh (x body)
           (== `(lambda (,x) ,body) exp)
           (== `(closure (lambda (,x) ,body) ,env) val)
           (== store-in store-out)
           (== addr-in addr-out)
           (symbolo x)
           (not-in-envo 'lambda env)))
        
        ((fresh (x e v addr store^)
           (== `(set! ,x ,e) exp)
           (== 'void val)
           (== (extend-store addr v store^) store-out)
           (symbolo x)
           (not-in-envo 'set! env)
           (lookup-varo x env addr)
           (eval-left-to-right-expo e env store-in addr-in store^ addr-out v)))

        ((fresh (a d v-a v-d store^ addr^)
           (== `(cons ,a ,d) exp)
           (== `(,v-a . ,v-d) val)
           (not-in-envo 'cons env)
           (eval-left-to-right-expo a env store-in addr-in store^ addr^ v-a)
           (eval-left-to-right-expo d env store^ addr^ store-out addr-out v-d)))
        
        ((fresh (rator rand x body env^ v store^ store^^ addr^ addr^^ new-env new-store new-addr)
           (== `(,rator ,rand) exp)
           (=/= 'quote rator)
           (== (extend-env x addr^^ env) new-env)
           (== (extend-store addr^^ v store^^) new-store)
           (== `(s ,addr^^) new-addr)
           (symbolo x)
           (eval-left-to-right-expo rator env store-in addr-in store^ addr^ `(closure (lambda (,x) ,body) ,env^))
           (eval-left-to-right-expo rand env store^ addr^ store^^ addr^^ v)
           (eval-left-to-right-expo body new-env new-store new-addr store-out addr-out val)))
        
        )))











(define eval-right-to-lefto
  (lambda (exp val)
    (fresh (store-out addr-out)
      (eval-right-to-left-expo exp '() '() 'z store-out addr-out val)
      (absento 'void exp))))

(define eval-right-to-left-expo
  (lambda (exp env store-in addr-in store-out addr-out val)
      (conde

        ((== `(quote ,val) exp)
         (== store-in store-out)
         (== addr-in addr-out)
         (not-in-envo 'quote env)
         (absento 'closure val))
        
        ((fresh (addr)
           (symbolo exp)
           (== store-in store-out)
           (== addr-in addr-out)
           (lookup-varo exp env addr)
           (lookup-addro addr store-in val)))

        ((fresh (x body)
           (== `(lambda (,x) ,body) exp)
           (== `(closure (lambda (,x) ,body) ,env) val)
           (== store-in store-out)
           (== addr-in addr-out)
           (symbolo x)
           (not-in-envo 'lambda env)))
        
        ((fresh (x e v addr store^)
           (== `(set! ,x ,e) exp)
           (== 'void val)
           (== (extend-store addr v store^) store-out)
           (symbolo x)
           (not-in-envo 'set! env)
           (lookup-varo x env addr)
           (eval-right-to-left-expo e env store-in addr-in store^ addr-out v)))

        ((fresh (a d v-a v-d store^ addr^)
           (== `(cons ,a ,d) exp)
           (== `(,v-a . ,v-d) val)
           (not-in-envo 'cons env)
           ;; we have to be careful here: want to feed the addr-in into the a evaluation,
           ;; even though store-in is fed into the d evaluation
           (eval-right-to-left-expo d env store-in addr^ store^ addr-out v-d)
           (eval-right-to-left-expo a env store^ addr-in store-out addr^ v-a)))
        
        ((fresh (rator rand x body env^ v store^ store^^ addr^ addr^^ new-env new-store new-addr)
           (== `(,rator ,rand) exp)
           (=/= 'quote rator)
           (== (extend-env x addr^^ env) new-env)
           (== (extend-store addr^^ v store^^) new-store)
           (== `(s ,addr^^) new-addr)
           (symbolo x)
           ;; we have to be careful here: want to feed the addr-in into the rator evaluation,
           ;; even though store-in is fed into the rand evaluation
           (eval-right-to-left-expo rand env store-in addr^ store^ addr^^ v)
           (eval-right-to-left-expo rator env store^ addr-in store^^ addr^ `(closure (lambda (,x) ,body) ,env^))
           (eval-right-to-left-expo body new-env new-store new-addr store-out addr-out val)))
        
        )))
