#lang racket/base

(provide sequence->vector)

(define (sequence->vector xs)
  (if (vector? xs)
      xs
      (for/vector ((x xs)) x)))
