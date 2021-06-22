#lang racket/base

(require racket/async-channel
         racket/contract
         racket/match
         racket/struct)

(provide
  metric?

  metric-set!
  metric-incr!
  metric-decr!
  metric-current
  metric-tagsets

  (contract-out
    [make-metric (-> string?
                     (one-of/c 'counter 'gauge 'histogram)
                     string?
                     (listof string?)
                     metric?)]))

;; XXX: dealing with timestamps?
(struct metric (name type description tag-names values ch th)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (o) 'metric)
      (lambda (o) (list (metric-name o)))))])

(define (make-metric name type description tag-names)
  (define metric-values (make-hash))
  (define ch (make-async-channel))
  (define th
    (thread/suspend-to-kill
     (init-metric-thread metric-values ch)))
  (metric name type description tag-names metric-values ch th))

(define (init-metric-thread metric-values ch)
  (define (run)
    (match (async-channel-get ch)
      [(list 'incr! tvs amt)
       (hash-update! metric-values tvs (lambda (v) (+ v amt)) 0)]
      [(list 'set! tvs new-val)
       (hash-set! metric-values tvs new-val)])
    (run))
  run)

(define (metric-send! m msg)
  (async-channel-put (metric-ch m) msg)
  (thread-resume (metric-th m) (current-thread)))

(define (metric-incr! m tvs [amt 1])
  (metric-send! m (list 'incr! tvs amt)))

(define (metric-decr! m tvs [amt 1])
  (metric-incr! m tvs (- amt)))

(define (metric-set! m tvs v)
  (metric-send! m (list 'set! tvs v)))

(define (metric-tagsets m)
  (hash-keys (metric-values m)))

(define (metric-current m tvs)
  (hash-ref (metric-values m) tvs 0))

(module+ test
  (require rackunit)

  (define (call-with-fresh-custodian thunk)
    (let ()
      (define custodian (make-custodian))
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-custodian custodian])
           (thunk)))
       (lambda ()
         (custodian-shutdown-all custodian)))))

  (test-case "test metric"
    (call-with-fresh-custodian
     (lambda ()
       (define m (make-metric 'test 'counter #f null))
       (metric-set! m null 42)
       (sleep 0.1)
       (check-equal? (metric-current m null) 42)

       (metric-incr! m null)
       (sleep 0.1)
       (check-equal? (metric-current m null) 43)

       (metric-decr! m null)
       (sleep 0.1)
       (check-equal? (metric-current m null) 42)

       (metric-decr! m null 2)
       (sleep 0.1)
       (check-equal? (metric-current m null) 40)

       (metric-incr! m null 5)
       (sleep 0.1)
       (check-equal? (metric-current m null) 45))))

  (test-case "test metric multiple tags"
    (call-with-fresh-custodian
     (lambda ()
       (define m (make-metric 'test 'counter #f '(status)))
       (define ok '("ok"))
       (define fail '("fail"))
       (metric-set! m ok 42)
       (sleep 0.1)
       (check-equal? (metric-current m ok) 42)

       (metric-incr! m ok)
       (sleep 0.1)
       (check-equal? (metric-current m ok) 43)

       (metric-decr! m ok)
       (sleep 0.1)
       (check-equal? (metric-current m ok) 42)

       (metric-decr! m ok 2)
       (sleep 0.1)
       (check-equal? (metric-current m ok) 40)

       (metric-incr! m ok 5)
       (sleep 0.1)
       (check-equal? (metric-current m ok) 45))))

  (test-case "kill safety"
    (define m
      (call-with-fresh-custodian
       (lambda () (make-metric 'test 'counter #f null))))
    (metric-incr! m null)
    (sleep 0.1)
    (check-false (thread-dead? (metric-th m)))
    (check-equal? (metric-current m null) 1)))

