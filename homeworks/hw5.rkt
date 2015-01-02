;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

(struct closure (env fun) #:transparent) ;; a closure is not in "source" programs; 
                                         ;; it is what functions evaluate to

;; Problem 1

;; (a).

(define (racketlist->mupllist rs)
  (cond [(null? rs) (aunit)]
        [#t (apair (car rs) (racketlist->mupllist (cdr rs)))]))
        
(define (mupllist->racketlist ms)
  (cond [(aunit? ms) null]
        [#t (cons (apair-e1 ms) (mupllist->racketlist (apair-e2 ms)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
(begin (printf "env=~v\n" env)
       (printf "var=~v\n" str)
       
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)])))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
;;(begin ();;print env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
               (begin (printf "v1=~s\n" v1)
                      (printf "v2=~s\n" v2)
                      (printf "?=~a\n" (int? v2))
                      (if (and (int? v1)
                               (int? v2))
                          (int (+ (int-num v1) 
                                  (int-num v2)))
                          (error "MUPL addition applied to non-number"))))]
        ;; ifgreater
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (> (int-num v1) (int-num v2))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env)))]

        ;; (eval-under-env (fun #f "x" (add (var "x") (int 7))) 
        ;;                            (list (cons "x" (int 3))
       #| [(fun? e)
         (begin (printf "e=~v\n" e)
         ((let
              ([arg
                
                (fun-formal e)
                ] 
               [body
                (begin 
                       (printf "body=~v\n" (fun-body e))
                       (printf "env=~v\n" (cons (cons (fun-formal e) (int 4)) env) )
                       (lambda(a) (eval-under-env
                                   (fun-body e) (list (cons (cons (fun-formal e) a) env)))))])
                                   ;;(cond [(null? env) 
                                     ;;     (list (cons (cons arg a) env))]
                                       ;;  [#t (cons (cons arg a) env)]
                                         ;;))))])
            (lambda()
              (begin (printf "param=~s\n" "p")
                     (body (int 4)))
              ))))]
        |#
        
       #| [(fun? e)
         (lambda(p)
           (eval-under-env 
            (fun-body e) 
            (cons (cons (fun-formal e) p) env)
                  ))]
        |#

        [(fun? e) (closure env e)]

       [(closure? e) e]
        ;; call
        [(call? e) 
         (let ([e1 (eval-under-env (call-funexp e) env)]
               [e2 (eval-under-env (call-actual e) env)])
           (begin (printf "actual-params=~s\n" e2)
                  (printf "body=~s\n" e1)
                  (printf "fun-name=~s\n"  (fun-nameopt (closure-fun e1)))
                  (printf "closure-fun=~s\n"  (closure-fun e1))
                  (printf "closure-env=~s\n"  (closure-env e1))
            (if (closure? e1)
                 (let ([static-env (closure-env e1)]
                       [fun1 (closure-fun e1)]
                       [farg-env (cons (fun-formal (closure-fun e1)) e2)]
                       [fname (fun-nameopt (closure-fun e1))])
                   (begin
                     (printf "static-env=~s\n"  static-env)
                     (printf "new-env1-then=~s\n" (cons (cons farg-env  (cons fname e1)) static-env))
                     (printf "new-env1-else=~s\n" (cons farg-env   static-env))
                     (if fname
                         (eval-under-env (fun-body fun1)
                                         (if (null? static-env)
                                             (list farg-env  (cons fname e1))
                                             (list (cons farg-env  (cons fname e1)) static-env)))
                         (eval-under-env (fun-body fun1)  (cons farg-env   static-env))
                         )
                     )
                 )
                 (error "MUPL addition applied to non-number"))
            ))]

        ;; mlet
        [(mlet? e)
         (let ([var  (mlet-var e)]
               [exp  (eval-under-env (mlet-e e) env)])
           ((lambda ()
              (eval-under-env (mlet-body e)
                              (if (null? env)
                                  (list (cons var exp))
                                  (cons (cons var exp) env)
                              ))))
          )]
        ;; apair        
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        ;; snd
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2  v)
               (error "MUPL snd applied to non pair")))]
        ;; fst
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL sdn applied to non pair")))]
        ;; aunit
        [(aunit? e) (aunit)]
        ;; isaunit
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
         
        [#t (error (format "bad MUPL expression: ~v" e))]))                      
;;)
;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) 
  (if (null? lstlst)
      (eval-under-env e2 env)
      (mlet (car (car lstlst)) (eval-under-env (cdr (car lstlst)) env)
            mlet* (cdr lstlst))))

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
