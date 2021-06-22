#lang racket/base

(require (for-syntax
          racket/base
          racket/sequence
          racket/syntax)
         metric/private/collector
         metric/private/metric
         syntax/parse/define)

(provide define-counter)

(begin-for-syntax
  (define (id->kw id)
    (string->keyword
     (symbol->string
      (syntax->datum id)))))

;; XXX: separate names for identifier and in the metric
(define-syntax-parse-rule
  (define-counter name:id
    {~optional {~seq #:collector collector-expr}
               #:defaults
               ([collector-expr #'(current-collector)])}
    description:str
    tag-names:id ...)

  #:with name-string
  (datum->syntax #'name (symbol->string (syntax->datum #'name)) #'name)

  #:with (tag-kws ...)
  (for/list ([t (in-syntax #'(tag-names ...))]) (id->kw t))

  #:with (tag-strs ...)
  (for/list ([t (in-syntax #'(tag-names ...))])
    (symbol->string (syntax->datum t)))

  #:with incr-name!
  (format-id #'name "~a-incr!" #'name #:source #'name)

  (begin
    (define name
      (make-metric name-string 'counter description '(tag-strs ...)))

    (collector-register-metric! collector-expr name)

    ;; XXX: inlining this or at least checking keywords at expand time could be
    ;; good
    (define (incr-proc! [amt 1] (~@ tag-kws tag-names) ...)
      (metric-incr! name (list tag-names ...) amt))
    (define-syntax-rule (incr-name! x (... ...)) (incr-proc! x (... ...)))))

(module+ test
  (require rackunit)

  (test-begin
   "initialize and check registration"
   (parameterize ([current-collector (make-collector)])
     (define-counter app_function_counts "" instance function)
     (check-equal? (collector-metrics (current-collector))
                   (list app_function_counts))
     (check-equal? (metric-tagsets app_function_counts) null)))

  (test-begin
   "check counter manipulation"
   (parameterize ([current-collector (make-collector)])
     (define-counter app_function_counts "" instance function)
     (for ([n 10])
       (app_function_counts-incr! #:instance "aaa" #:function "fibonacci"))
     (sleep 0.2)
     (check-equal? (metric-tagsets app_function_counts)
                   (list (list "aaa" "fibonacci")))
     (check-equal? (metric-current app_function_counts (list "aaa" "fibonacci")) 10))))

