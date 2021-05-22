#lang info
(define name "metric")
(define pkg-desc "Description Here")
(define pkg-authors '(samdphillips@gmail.com))
(define version "0.0")

(define collection "metric")
(define deps '("base" "metric-lib"))
(define implies '("metric-lib"))
(define build-deps '("racket-doc" "scribble-lib"))
(define scribblings '(("scribblings/metric-manual.scrbl" ())))
