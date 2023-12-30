;; F(3n) = 4 * F(3*n-3)) + F(3*n-6))
(define answer
  (do [(x 2 y)
       (y 8 (+ x (* 4 y)))
       (s 2 (+ s y))]
    ((> y 4000000) s)))
