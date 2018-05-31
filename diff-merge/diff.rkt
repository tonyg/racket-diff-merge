#lang racket/base
;; Text diff algorithm after Myers 1986 and Ukkonen 1985, following
;; Levente Uzonyi's Squeak Smalltalk implementation at
;; http://squeaksource.com/DiffMerge.html
;;
;; E. W. Myers, “An O(ND) difference algorithm and its variations,”
;; Algorithmica, vol. 1, no. 1–4, pp. 251–266, Nov. 1986.
;;
;; E. Ukkonen, “Algorithms for approximate string matching,” Inf.
;; Control, vol. 64, no. 1–3, pp. 100–118, Jan. 1985.

;; Copyright (C) 2011–2018 Tony Garnock-Jones <tonyg@leastfixedpoint.com>

(provide (struct-out difference)
         diff-indices)

(require racket/match)
(require "sequence.rkt")

(struct difference (old-lhs old-len new-lhs new-len) #:prefab)

(define (diff-indices xs0 ys0)
  (define xs (sequence->vector xs0))
  (define ys (sequence->vector ys0))
  (let loop ((i -1)
             (j -1)
             (matches (append (longest-common-subsequence* xs ys)
                              (list (cons (vector-length xs) (vector-length ys))))))
    (match matches
      ['() '()]
      [(cons (cons mi mj) rest)
       (define li (- mi i 1))
       (define lj (- mj j 1))
       (if (or (positive? li) (positive? lj))
           (cons (difference (+ i 1) li (+ j 1) lj) (loop mi mj rest))
           (loop mi mj rest))])))

(define (longest-common-subsequence xs ys)
  (longest-common-subsequence* (sequence->vector xs) (sequence->vector ys)))

(define (longest-common-subsequence* xs ys)
  (define xs-length (vector-length xs))
  (define ys-length (vector-length ys))
  (define total-length (+ xs-length ys-length))
  (define storage-length (+ 1 (* 2 total-length)))
  (define frontier (make-vector storage-length 0))
  (define candidates (make-vector storage-length '()))
  (let/ec return
    (for ((d (in-range 0 (+ total-length 1))))
      (for ((k (in-range (- d) (+ d 1) 2)))
        (define-values (index x)
          (if (or (= k (- d))
                  (and (not (= k d))
                       (< (vector-ref frontier (+ total-length k -1))
                          (vector-ref frontier (+ total-length k 1)))))
              (values (+ total-length k 1) (vector-ref frontier (+ total-length k 1)))
              (values (+ total-length k -1) (+ (vector-ref frontier (+ total-length k -1)) 1))))
        (let loop ((x x) (y (- x k)) (chain (vector-ref candidates index)))
          (cond
           [(and (< x xs-length) (< y ys-length) (equal? (vector-ref xs x) (vector-ref ys y)))
            (loop (+ x 1) (+ y 1) (cons (cons x y) chain))]
           [(and (>= x xs-length) (>= y ys-length))
            (return (reverse chain))]
           [else
            (vector-set! frontier (+ total-length k) x)
            (vector-set! candidates (+ total-length k) chain)]))))))

(module+ test
  (require rackunit)

  ;; (define (test-example xs ys)
  ;;   (printf "~v\n" (longest-common-subsequence xs ys))
  ;;   (printf "~v\n" (diff-indices xs ys)))
  ;; (test-example "The red brown fox jumped over the rolling log"
  ;;               "The brown spotted fox leaped over the rolling log")

  (check-equal? (diff-indices "The red brown fox jumped over the rolling log"
                              "The brown spotted fox leaped over the rolling log")
                (list (difference 4 4 4 0)
                      (difference 14 0 10 8)
                      (difference 18 3 22 3)))

  (check-equal? (longest-common-subsequence "acbcaca" "bcbcacb")
                '((1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5)))
  (check-equal? (longest-common-subsequence "bcbcacb" "acbcaca")
                '((1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5)))
  (check-equal? (longest-common-subsequence "acba" "bcbb")
                '((1 . 1) (2 . 2)))
  (check-equal? (longest-common-subsequence "abcabba" "cbabac")
                '((2 . 0) (3 . 2) (4 . 3) (6 . 4)))
  (check-equal? (longest-common-subsequence "cbabac" "abcabba")
                '((1 . 1) (2 . 3) (3 . 4) (4 . 6)))

  (check-equal? (longest-common-subsequence
                 (vector (vector 1 1 1) (vector 1 1 1) (vector 1 1 1) (vector 1 1 1))
                 (vector (vector 1 1 1) (vector 2 2 2) (vector 1 1 1) (vector 4 4 4)))
                '((0 . 0) (1 . 2)))
  (check-equal? (diff-indices
                 (vector (vector 1 1 1) (vector 1 1 1) (vector 1 1 1) (vector 1 1 1))
                 (vector (vector 1 1 1) (vector 2 2 2) (vector 1 1 1) (vector 4 4 4)))
                (list (difference 1 0 1 1)
                      (difference 2 2 3 1)))

  (check-equal? (longest-common-subsequence '(a b c) '(d e f)) '())
  (check-equal? (diff-indices '(a b c) '(d e f))
                (list (difference 0 3 0 3)))

  ;; (let ((size 400))
  ;;   (local-require profile)
  ;;   (profile-thunk
  ;;    (lambda ()
  ;;      (diff-indices (make-vector size 'x)
  ;;                    (let ((v (make-vector size 'x)))
  ;;                      (vector-set! v 0 'a)
  ;;                      (vector-set! v 1 'b)
  ;;                      (vector-set! v 2 'c)
  ;;                      v)))))
  )
