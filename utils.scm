(define (last-element my-list)
  (let ((reversed-list (reverse my-list)))
    (car reversed-list)))

(define-syntax ->>
  (syntax-rules ()
    [(_ value) value]
    [(_ value (fn args ...) rest ...)
      (->> (fn args ... value) rest ...)]
    [(_ value fn rest ...)
      (->> (fn value) rest ...)]))

(define-syntax ->
  (syntax-rules ()
    [(_ value) value]
    [(_ value (fn args ...) rest ...)
      (-> (fn value args ...) rest ...)]
    [(_ value fn rest ...)
      (-> (fn value) rest ...)]))

(define (subsets ls)
  (if (null? ls)
    '(())
    (let [(x (car ls))
          (ss (subsets (cdr ls)))]
      (append ss
              (map (lambda (ys) (cons x ys)) ss)))))

(define (make-queue)
  (let [(end '())]
    (cons end end)))

(define (putq! q v)
  (let [(elem (cons v '()))]
    (if (null? (cdr q))
        (begin (set-car! q elem)
               (set-cdr! q elem))
        (begin (set-cdr! (cdr q) elem)
               (set-cdr! q elem)))))

(define (pushq! q v)
  (set-car! q (cons v (car q))))

(define (getq q)
  (car (car q)))

(define (delq! q)
  (if (eqv? (car q) (cdr q))
    (let [(end '())]
      (set-car! q end)
      (set-cdr! q end))
    (set-car! q (cdr (car q)))))

(define (listq q)
  (car q))

(define (first-true-vec vec a b v)
  (do [(i a (+ i 1))]
    ((or (>= i b) 
          (eqv? v (vector-ref vec i)))
     (if (< i b) i (- 1)))))

(define (primes n)
  ;; Sift the sieve
  (let [(is_prime (make-vector n #t))
        (_primes (make-queue))]
    (vector-set! is_prime 0 #f)
    (vector-set! is_prime 1 #f)
    (do [(i 2 (first-true-vec is_prime (+ i 1) n #t))]
      ((or (< i 0)
           (>= i n))
       (listq _primes))
      (do [(j i (+ j i))]
        ((>= j n))
        (vector-set! is_prime j #f))
      (putq! _primes i))))

(define (prime-factors n)
  (do [(_primes (primes (inexact->exact (ceiling (sqrt n)))) (cdr _primes))
       (factors (make-queue))
       (i n)]
    ((or (null? _primes)
         (<= i 1))
     (when (null? _primes)
       (putq! factors i))
     (listq factors))
    (do [(p (car _primes))]
      ((< 0 (modulo i p)))
      (set! i (/ i p))
      (putq! factors p))))
