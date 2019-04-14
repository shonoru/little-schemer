#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))


(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? a (car lat)) (cdr lat))
              (else (cons (car lat) (rember a (cdr lat)))))))))

;refactored function

(define rember2
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember2 a (cdr lat)))))))

(define firsts
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car (car lat)) (firsts (cdr lat)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons old (cons new (cdr lat))))
              (else (cons (car lat) (insertR new old (cdr lat)))))))))

;(insertR 'jalapeno 'and '(tacos tamales and salsa))
;(insertR 'e 'd '(a b c d f g))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons new lat))
              (else (cons (car lat) (insertL new old (cdr lat)))))))))

;(insertL 'c 'd '(a b d e f g))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons new (cdr lat)))
              (else (cons (car lat) (subst new old (cdr lat)))))))))

;(subst 'topping 'fudge '(ice cream with fudge for dessert))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? o1 (car lat)) (cons new (cdr lat)))
              ((eq? o2 (car lat)) (cons new (cdr lat)))
              (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(define subst2ref
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
              (else (cons (car lat) (subst2ref new o1 o2 (cdr lat)))))))))

;(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
;(subst2ref 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? a (car lat)) (multirember a (cdr lat)))
              (else (cons (car lat) (multirember a (cdr lat)))))))))

;(multirember 'cup '(coffee cup tea cup and hick cup))\

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
              (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

;(multiinsertR 'a 'b  '(c b c b))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
              (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

;(multiinsertL 'a 'c  '(c b c b))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
              (else (cons (car lat) (multisubst new old (cdr lat)))))))))

;(multisubst 'a 'some '(some b c some b c))

(define o+
  (lambda (n1 n2)
    (cond
      ((zero? n2) n1)
      (else (add1 (o+ n1 (sub1 n2)))))))

;(o+ 1 2) 

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

;(o- 2 1)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

;(addtup '(1 2 3))

(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (o* n (sub1 m)))))))

;(o* 3 3)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ;((and (null? tup1) (null? tup2)) '())
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons
             (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

;(tup+ '(2 3) '(4 6))
;(tup+ '(1 2 3) '(2 1))

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

;(o> 1 2)
;(o> 2 2)
;(o> 2 1)

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

;(o< 1 2) ; #t
;(o< 2 2) ; #f
;(o< 2 1) ; #f

(define o=
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))

;(o= 1 1)
;(o= 1 2)

(define o^
  (lambda (z w)
    (cond
      ((zero? w) 1)
      (else (o* z (o^ z (sub1 w)))))))
             
;(o^ 2 3)
;(o^ 1 1)
;(o^ 1 0)

(define o/
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))

;(o/ 15 4)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

;(length '(a b c d e f)) ; 6

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

;(pick 4 '(a b c d e)) ; 'd

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;(rempick 3 '(hotdog with hot mustard))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

;(no-nums '(5 pears 6 prunes 9 dates))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))
                        
;(all-nums '(5 pears 6 prunes 9 dates))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

;(eqan? 'a 'a)
;(eqan? 1 1)
;(eqan? 'a 'b)
;(eqan? 1 2)

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ((eq? a (car lat)) (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))
;(occur 'a '(a b c)) ; 1
;(occur 'a '(a b a c a d))

;(define one?
;  (lambda (n)
;    (eqan? n 1)))

#;(define one?
  (lambda (n)
    (cond
      (else (= n 1)))))

(define one?
  (lambda (n)
    (= n 1)))
;(one? 1) ; #t
;(one? 2) ; #f

; rempick with 'one?'
(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))


;(rempick2 3 '(lemon meringue salty pie)) ; '(lemon meringue pie)
; End of 4th chapter ^^v


#|

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? a (car lat)) (cdr lat))
              (else (cons (car lat) (rember a (cdr lat)))))))))

|#


(define rember*
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat)) (cond
                           ((eq? a (car lat)) (rember* a (cdr lat)))
                           (else (cons (car lat) (rember* a (cdr lat))))))
      (else (cons (rember* a (car lat)) (rember* a (cdr lat)))))))

;(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
;(rember* 'sauce '(((tomato sauce)) ((bean) sauce)(and ((flying)) sauce)))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
                         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

;(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) (cond
                         ((eq? a (car l)) (add1 (occur* a (cdr l)))) 
                         (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

;(occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                        ((eq? old (car l)) (cons new (subst* new old (cdr l))))
                        (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

;(subst* 'orange 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
                         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))
             
;(insertL* 'pecker 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eq? a (car l)) (member* a (cdr l))))
    (else (or (member* a (car l)) (member* a (cdr l)))))))

;(member* 'chips '((potato) (chips ((with) fish) (chips))))

(define leftmost
  (lambda (l)
    (cond
      ((lat? l) (car l))
      (else (leftmost (car l))))))

;(leftmost '((potato) (chips ((with) fish) (chips))))
;(leftmost '(((hot) tune (and)) cheese))
;(leftmost '())

#|
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)(null? l2)) #t)
      ((and (null? l1)(
      ()
      ()
      ()
      ()
      ()
      ()
      (else)
|#

;(eqlist? '(strawberry ice cream) '(strawberry ice cream))

