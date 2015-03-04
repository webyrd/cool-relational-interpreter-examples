(load "../mk/mk.scm")

;; supports variadic lambda: (lambda x x)


;; letrec is based on Dan Friedman's code, using the "half-closure"
;; approach from Reynold's definitional interpreters


(define lookupo
  (lambda (x env t)
    (conde
      ((fresh (y v rest)
       (== `(ext-env ,y ,v ,rest) env)
       (conde
         ((== y x) (== v t))
         ((=/= y x) (lookupo x rest t)))))

      ((fresh (defs rest)
         (== `(ext-rec ,defs ,rest) env)
         (lookup-ext-reco x defs env rest t)))
      
      )))

(define lookup-ext-reco
  (lambda (x defs env rest t)
    (fresh (y lam-exp others)
      (conde
        ((== '() defs) (lookupo x rest t))
        ((== `((,y ,lam-exp) . ,others) defs)
         (conde
           ((== y x) (== `(closure ,lam-exp ,env) t))
           ((=/= y x) (lookup-ext-reco x others env rest t))))))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((== '() env))
      ((fresh (y v rest)
         (== `(ext-env ,y ,v ,rest) env)
         (=/= y x)
         (not-in-envo x rest)))

      ((fresh (defs rest)
         (== `(ext-rec ,defs ,rest) env)
         (not-in-defso x defs)
         (not-in-envo x rest)))

      )))

(define not-in-defso
  (lambda (x defs)
    (conde
      ((== '() defs))
      ((fresh (y lam-exp others)
         (== `((,y ,lam-exp) . ,others) defs)
         (=/= y x)
         (not-in-defso x others))))))

(define eval-listo
  (lambda (exp env val)
    (conde
      ((== '() exp)
       (== '() val))
      ((fresh (a d v-a v-d)
         (== `(,a . ,d) exp)
         (== `(,v-a . ,v-d) val)
         (eval-expo a env v-a)
         (eval-listo d env v-d))))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(define list-of-symbolso
  (lambda (los)
    (conde
      ((== '() los))
      ((fresh (a d)
         (== `(,a . ,d) los)
         (symbolo a)
         (list-of-symbolso d))))))


(define listo
  (lambda (ls)
    (conde
      ((== '() ls))
      ((fresh (a d)
         (== `(,a . ,d) ls)
         (listo d))))))

(define evalo
  (lambda (exp val)
    (eval-expo exp '() val)))

(define eval-expo
  (lambda (exp env val)
    (conde

      ((== `(quote ,val) exp)
       (absento 'closure val)
       (not-in-envo 'quote env))

      ((symbolo exp) (lookupo exp env val))
      
      ;; should possibly combine these lambda clauses, application clauses, apply clauses, and letrec clauses

      ((fresh (x body)
         (== `(lambda ,x ,body) exp)
         (== `(closure (lambda ,x ,body) ,env) val)
         (symbolo x)
         (not-in-envo 'lambda env)))
      
      ((fresh (x* body)
         (== `(lambda ,x* ,body) exp)
         (== `(closure (lambda ,x* ,body) ,env) val)
         (list-of-symbolso x*)
         (not-in-envo 'lambda env)))
      
      ;; apply for variadic procedure
      ((fresh (e e* x body env^ a* res)
         (== `(apply ,e ,e*) exp)
         (not-in-envo 'apply env)
         (symbolo x)
         (== `(ext-env ,x ,a* ,env^) res)
         (eval-expo e env `(closure (lambda ,x ,body) ,env^))
         (eval-expo e* env a*)
         (listo a*)
         (eval-expo body res val)))

      ;; apply for mult-argument procedure
      ((fresh (e e* x x* body env^ a* res)
         (== `(apply ,e ,e*) exp)
         (not-in-envo 'apply env)
         (symbolo x)
         (ext-env*o `(,x . ,x*) a* env^ res)
         (eval-expo e env `(closure (lambda (,x . ,x*) ,body) ,env^))
         (eval-expo e* env a*)
         (listo a*)
         (eval-expo body res val)))
      
      ((fresh (a*)
         (== `(list . ,a*) exp)
         (not-in-envo 'list env)
         (eval-listo a* env val)))
      
      ((fresh (rator x rands body env^ a* res)
         (== `(,rator . ,rands) exp)
         (symbolo x)
         (== `(ext-env ,x ,a* ,env^) res)
         (eval-expo rator env `(closure (lambda ,x ,body) ,env^))
         
         (eval-expo body res val) ;; perfect example of two serious
                                  ;; calls in which it isn't clear
                                  ;; which one should come first         
         (eval-listo rands env a*)))
      
      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) exp)
         (eval-expo rator env `(closure (lambda ,x* ,body) ,env^))
         (eval-listo rands env a*)
         (ext-env*o x* a* env^ res)
         (eval-expo body res val)))

      ((fresh (p-name x body letrec-body)
         (== `(letrec ((,p-name (lambda ,x ,body))) ;; single-function variadic letrec version
                ,letrec-body)
             exp)
         (symbolo x)
         (not-in-envo 'letrec env)
         (eval-expo letrec-body
                    `(ext-rec ((,p-name (lambda ,x ,body))) ,env)
                    val)))
      
      ((fresh (p-name x* body letrec-body)
         (== `(letrec ((,p-name (lambda ,x* ,body))) ;; single-function multiple-argument letrec version
                ,letrec-body)
             exp)
         (list-of-symbolso x*)
         (not-in-envo 'letrec env)
         (eval-expo letrec-body
                    `(ext-rec ((,p-name (lambda ,x* ,body))) ,env)
                    val)))
      
      ;;; don't comment this out accidentally!!!
      ((prim-expo exp env val))
            
      )))

(define ext-env*o
  (lambda (x* a* env out)
    (conde
      ((== '() x*) (== '() a*) (== env out))
      ((fresh (x a dx* da* env2)
         (== `(,x . ,dx*) x*)
         (== `(,a . ,da*) a*)
         (== `(ext-env ,x ,a ,env) env2)
         (symbolo x)
         (ext-env*o dx* da* env2 out))))))

(define prim-expo
  (lambda (exp env val)
    (conde
      ((boolean-primo exp env val))
      ((null?-primo exp env val))
      ((symbol?-primo exp env val))
      ((not-primo exp env val))
      ((car-primo exp env val))
      ((cdr-primo exp env val))
      ((cons-primo exp env val))
      ((equal?-primo exp env val))
      ((if-primo exp env val)))))

(define boolean-primo
  (lambda (exp env val)
    (conde
      ((== #t exp) (== #t val))
      ((== #f exp) (== #f val)))))

(define equal?-primo
  (lambda (exp env val)
    (fresh (e1 e2 v1 v2)
      (== `(equal? ,e1 ,e2) exp)
      (conde
        ((== v1 v2) (== #t val))
        ((=/= v1 v2) (== #f val)))
      (not-in-envo 'equal? env)
      (eval-expo e1 env v1)
      (eval-expo e2 env v2))))

(define cons-primo
  (lambda (exp env val)
    (fresh (a d v-a v-d)
      (== `(cons ,a ,d) exp)
      (== `(,v-a . ,v-d) val)
      (not-in-envo 'cons env)
      (eval-expo a env v-a)
      (eval-expo d env v-d))))

(define car-primo
  (lambda (exp env val)
    (fresh (p a d)
      (== `(car ,p) exp)
      (== a val)
      (=/= 'closure a)
      (not-in-envo 'car env)
      (eval-expo p env `(,a . ,d)))))

(define cdr-primo
  (lambda (exp env val)
    (fresh (p a d)
      (== `(cdr ,p) exp)
      (== d val)
      (=/= 'closure a)
      (not-in-envo 'cdr env)
      (eval-expo p env `(,a . ,d)))))

(define not-primo
  (lambda (exp env val)
    (fresh (e b)
      (== `(not ,e) exp)
      (conde
        ((=/= #f b) (== #f val))
        ((== #f b) (== #t val)))         
      (not-in-envo 'not env)
      (eval-expo e env b))))

(define symbol?-primo
  (lambda (exp env val)
    (fresh (e v)
      (== `(symbol? ,e) exp)
      (conde
        ((symbolo v) (== #t val))
        ((numbero v) (== #f val))
        ((fresh (a d)
           (== `(,a . ,d) v)
           (== #f val))))
      (not-in-envo 'symbol? env)
      (eval-expo e env v))))

(define null?-primo
  (lambda (exp env val)
    (fresh (e v)
      (== `(null? ,e) exp)
      (conde
        ((== '() v) (== #t val))
        ((=/= '() v) (== #f val)))
      (not-in-envo 'null? env)
      (eval-expo e env v))))

(define if-primo
  (lambda (exp env val)
    (fresh (e1 e2 e3 t)
      (== `(if ,e1 ,e2 ,e3) exp)
      (not-in-envo 'if env)
      (eval-expo e1 env t)
      (conde
        ((=/= #f t) (eval-expo e2 env val))
        ((== #f t) (eval-expo e3 env val))))))
