#lang racket/base

(require racket/contract
         metric/private/metric)

(provide
  collector?
  collector-metrics
  current-collector
  make-collector

  (contract-out
    [collector-register-metric! (-> collector? metric? any)]))

(struct collector (metrics) #:mutable)

(define (make-collector) (collector null))

(define (collector-register-metric! col m)
  (set-collector-metrics! col (cons m (collector-metrics col))))

(define current-collector
  (make-parameter (make-collector)))

