#lang racket/base

(require racket/async-channel
         racket/contract
         racket/match
         racket/struct)

(provide
 (contract-out
  [metric-value?        (-> any/c boolean?)]
  [make-metric-value    (->* () (real?) metric-value?)]
  [metric-value-current (-> metric-value? real?)]
  [metric-value-set!    (-> metric-value? real? any)]
  [metric-value-incr!   (->* (metric-value?) (real?) any)]
  [metric-value-decr!   (->* (metric-value?) (real?) any)]))

(module+ test
  (require rackunit))

(struct metric-value (box ch th)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (o) 'metric-value)
      (lambda (o) (list (metric-value-current o)))))])

(define (make-metric-value [init 0])
  (define ch (make-async-channel))
  (define b (box init))
  (define th
    (thread/suspend-to-kill
     (start-metric-value-server ch b)))
  (metric-value b ch th))

(define (metric-value-current mv)
  (unbox (metric-value-box mv)))

(define (metric-value-send! mv msg)
  (async-channel-put (metric-value-ch mv) msg)
  (thread-resume (metric-value-th mv) (current-custodian)))

(define (metric-value-set! mv val)
  (metric-value-send! mv (list 'set! val)))

(define (metric-value-incr! mv [amt 1])
  (metric-value-send! mv (list 'incr! amt)))

(define (metric-value-decr! mv [amt 1])
  (metric-value-incr! mv (- amt)))

(define (start-metric-value-server ch value-box)
  (define-syntax-rule (set-value! new-val)
    (set-box! value-box new-val))
  (define-syntax-rule (incr-value! amt)
    (let ([v (unbox value-box)])
      (set-box! value-box (+ amt v))))
  (define (run)
    (match (async-channel-get ch)
      [(list 'incr! amt) (incr-value! amt)]
      [(list 'set! val)  (set-value! val)])
    (run))
  run)

(module+ test
  (define-syntax-rule (with-custodian body ...)
    (let ()
      (define custodian (make-custodian))
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-custodian custodian])
           body ...))
       (lambda ()
         (custodian-shutdown-all custodian)))))

  (test-case "test metric-value messages"
    (with-custodian
      (define b (box 0))
      (define ch (make-async-channel))
      (define th (thread (start-metric-value-server ch b)))
      (define-syntax-rule (msg-check msg new-val)
        (begin
          (async-channel-put ch 'msg)
          (sleep 0.1)
          (check-false (thread-dead? th))
          (check-equal? (unbox b) new-val)))
      (check-false (thread-dead? th))
      (check-equal? (unbox b) 0)
      (msg-check (set! 42) 42)
      (msg-check (incr! 1) 43)
      (msg-check (incr! -1) 42)))

  (test-case "test metric-value"
    (with-custodian
      (define mv (make-metric-value))
      (check-true (zero? (metric-value-current mv)))
      (metric-value-set! mv 42)
      (sleep 0.1)
      (check-equal? (metric-value-current mv) 42)

      (metric-value-incr! mv)
      (sleep 0.1)
      (check-equal? (metric-value-current mv) 43)

      (metric-value-decr! mv)
      (sleep 0.1)
      (check-equal? (metric-value-current mv) 42)

      (metric-value-decr! mv 2)
      (sleep 0.1)
      (check-equal? (metric-value-current mv) 40)

      (metric-value-incr! mv 5)
      (sleep 0.1)
      (check-equal? (metric-value-current mv) 45)))

  (test-case "kill safety"
    (define mv
      (with-custodian
        (make-metric-value)))
    (metric-value-incr! mv)
    (sleep 0.1)
    (check-false (thread-dead? (metric-value-th mv)))
    (check-equal? (metric-value-current mv) 1)))
