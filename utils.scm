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
