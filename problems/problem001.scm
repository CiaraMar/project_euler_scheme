(load "utils.scm")

(define (sum_range n)
  (-> n
       (+ 1)
       (* n)
       (/ 2)))

(define (mult_range m n)
  (->> m
       (quotient n)
       (sum_range)
       (* m)))

(define (sum_multiples n ls)
  (define (comb xs)
    (if (null? xs)
      0
      (let [(sign (if (odd? (length xs))
                    1
                    (- 1)))
            (mult (apply lcm xs))]
        (* sign 
           (mult_range mult n)))))
  (fold-left + 0 (map comb (subsets ls))))

(define answer (sum_multiples 999 '(3 5)))
