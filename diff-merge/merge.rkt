#lang racket/base
;; 3-way merge, using the "diff3" algorithm.
;;
;; Sanjeev Khanna, Keshav Kunal, and Benjamin C. Pierce. "A Formal
;; Investigation of Diff3." In Arvind and Prasad, editors, Foundations
;; of Software Technology and Theoretical Computer Science (FSTTCS),
;; December 2007.
;;
;; (http://www.cis.upenn.edu/~bcpierce/papers/diff3-short.pdf)

;; Copyright (C) 2018 Tony Garnock-Jones <tonyg@leastfixedpoint.com>

(provide (struct-out copy-instruction)
         (struct-out conflict-instruction)
         diff3-merge-instructions

         (struct-out ok-block)
         (struct-out conflict-block)
         diff3-merge)

(require racket/match)
(require racket/sequence)
(require racket/vector)
(require "diff.rkt")
(require "sequence.rkt")

(module+ test (require rackunit))

;;---------------------------------------------------------------------------

(struct copy-instruction (side lhs len) #:prefab)
(struct conflict-instruction (a-lhs a-len o-lhs o-len b-lhs b-len) #:prefab)

(struct hunk (side base-lhs base-len other-lhs other-len) #:prefab)

(define (hunk-base-rhs h)
  (+ (hunk-base-lhs h) (hunk-base-len h)))

(define (diff3-merge-instructions a o b)
  ;; Given three sequences, A, O, and B, where both A and B are
  ;; independently derived from O, returns a sequence of
  ;; *instructions* for building a merged file.

  (define o-length (sequence-length o))
  (define a-length (sequence-length a))
  (define b-length (sequence-length b))

  (define hunks (extract-sorted-hunks a o b))
  (define hunk-count (vector-length hunks))

  (define (merge-hunks side lo hi)
    (define x-length (match side [0 a-length] [2 b-length]))
    (for/fold [(acc (hunk side
                          o-length (- -1 o-length)
                          x-length (- -1 x-length)))]
              [(i (in-range lo hi))]
      (define h (vector-ref hunks i))
      (if (= (hunk-side h) side)
          (merge-hunk acc h)
          acc)))

  (let loop ((result '()) (common-offset 0) (current-hunk-index 0))
    (cond
      [(= current-hunk-index hunk-count)
       (reverse (push-copy result 1 common-offset (- o-length common-offset)))]
      [else
       (define h (vector-ref hunks current-hunk-index))
       (define base-lhs (hunk-base-lhs h))
       (define-values (base-rhs rightmost-hunk-index) (find-overlap-end hunks current-hunk-index))

       (let* ((result (push-copy result 1 common-offset (- base-lhs common-offset)))
              (result
               (cond [(= current-hunk-index rightmost-hunk-index)
                      ;; The "overlap" was only one hunk long, meaning that
                      ;; there's no conflict here. Either a and o were the
                      ;; same, or b and o were the same.
                      (push-copy result (hunk-side h) (hunk-other-lhs h) (hunk-other-len h))]
                     [else
                      ;; A proper conflict. Determine the extents of the
                      ;; regions involved from a, o and b. Effectively merge
                      ;; all the hunks on the left into one giant hunk, and
                      ;; do the same for the right; then, correct for skew
                      ;; in the regions of o that each side changed, and
                      ;; report appropriate spans for the three sides.
                      (define ah (merge-hunks 0 current-hunk-index (+ rightmost-hunk-index 1)))
                      (define bh (merge-hunks 2 current-hunk-index (+ rightmost-hunk-index 1)))
                      (define olen (- base-rhs base-lhs))
                      (define alhs (+ (hunk-other-lhs ah) (- base-lhs (hunk-base-lhs ah))))
                      (define alen (+ (hunk-other-len ah) (- olen (hunk-base-len ah))))
                      (define blhs (+ (hunk-other-lhs bh) (- base-lhs (hunk-base-lhs bh))))
                      (define blen (+ (hunk-other-len bh) (- olen (hunk-base-len bh))))
                      (push-conflict result alhs alen base-lhs olen blhs blen)])))
         (loop result base-rhs (+ rightmost-hunk-index 1)))])))

(define (extract-sorted-hunks a o b)
  (list->vector
   (sort (append (map (indices->hunk 0) (diff-indices o a))
                 (map (indices->hunk 2) (diff-indices o b)))
         < #:key hunk-base-lhs)))

(define ((indices->hunk side) i)
  (match-define (difference bp bl op ol) i)
  (hunk side bp bl op ol))

(define (find-overlap-end hunks hunk-index)
  (define hunk-count (vector-length hunks))
  (let find-overlap ((base-rhs (hunk-base-rhs (vector-ref hunks hunk-index)))
                     (rightmost-hunk-index hunk-index))
    (define next-hunk-index (+ rightmost-hunk-index 1))
    (if (= next-hunk-index hunk-count)
        (values base-rhs rightmost-hunk-index)
        (let ((candidate (vector-ref hunks next-hunk-index)))
          (if (> (hunk-base-lhs candidate) base-rhs)
              (values base-rhs rightmost-hunk-index)
              (find-overlap (max base-rhs (hunk-base-rhs candidate))
                            next-hunk-index))))))

(define (merge-hunk h1 h2)
  (match-define (hunk side1 bp1 bl1 op1 ol1) h1)
  (match-define (hunk side2 bp2 bl2 op2 ol2) h2)
  (when (not (= side1 side2))
    (error 'merge-hunk "Internal error: attempt to merge incoherent hunks: ~v ~v" h1 h2))
  (define bp (min bp1 bp2))
  (define op (min op1 op2))
  (hunk side1 bp (- (max (+ bp1 bl1) (+ bp2 bl2)) bp) op (- (max (+ op1 ol1) (+ op2 ol2)) op)))

(define (push-copy result side lhs len)
  (if (positive? len)
      (cons (copy-instruction side lhs len) result)
      result))

(define (push-conflict result alhs alen olhs olen blhs blen)
  (cons (conflict-instruction alhs alen olhs olen blhs blen) result))

;;---------------------------------------------------------------------------

(struct ok-block (items) #:prefab)
(struct conflict-block (a-items a-index o-items o-index b-items b-index) #:prefab)

(define (diff3-merge a0 o0 b0
                     #:exclude-false-conflicts? [exclude-false-conflicts? #f]
                     #:convert-block [convert-block values])
  ;; Applies the output of diff3-merge-instructions to actually
  ;; construct the merged file; the returned result alternates between
  ;; "ok" and "conflict" blocks.
  (define a (sequence->vector a0))
  (define o (sequence->vector o0))
  (define b (sequence->vector b0))
  (define (side->sequence side) (match side [0 a] [1 o] [2 b]))
  (coalesce-adjacent-ok-blocks
   convert-block
   (reverse
    (for/fold [(blocks-rev '())]
              [(instruction (in-list (diff3-merge-instructions a o b)))]
      (match instruction
        [(copy-instruction side lhs len)
         (cons (ok-block (vector-copy (side->sequence side) lhs (+ lhs len))) blocks-rev)]
        [(and i (conflict-instruction alhs alen _ _ _ _))
         #:when (and exclude-false-conflicts? (not (true-conflict? a b i)))
         (cons (ok-block (vector-copy a alhs (+ alhs alen))) blocks-rev)]
        [(conflict-instruction alhs alen olhs olen blhs blen)
         (cons (conflict-block (vector-copy a alhs (+ alhs alen)) alhs
                               (vector-copy o olhs (+ olhs olen)) olhs
                               (vector-copy b blhs (+ blhs blen)) blhs)
               blocks-rev)])))))

(define (coalesce-adjacent-ok-blocks convert-block blocks)
  (let walk ((blocks blocks))
    (match blocks
      ['()
       '()]
      [(list* (ok-block a) (ok-block b) rest)
       (walk (list* (ok-block (vector-append a b)) rest))]
      [(list* (ok-block a) rest)
       (list* (ok-block (convert-block a)) (walk rest))]
      [(list* (conflict-block a ai o oi b bi) rest)
       (list* (conflict-block (convert-block a) ai (convert-block o) oi (convert-block b) bi)
              (walk rest))])))

(define (true-conflict? a b i)
  (match-define (conflict-instruction alhs alen _ _ blhs blen) i)
  (or (not (= alen blen))
      (for/or [(j (in-range alen))]
        (not (equal? (vector-ref a (+ j alhs))
                     (vector-ref b (+ j blhs)))))))

;;---------------------------------------------------------------------------

(module+ test
  (define (test-diff3-merge a o b [convert-block values])
    (define instructions (diff3-merge-instructions a o b))
    (define merged (diff3-merge a o b #:convert-block convert-block))
    (define merged/no-false-conflicts (diff3-merge a o b #:exclude-false-conflicts? #t
                                                   #:convert-block convert-block))
    (if (equal? merged merged/no-false-conflicts)
        (list instructions merged)
        (list instructions merged merged/no-false-conflicts)))

  (check-equal? (test-diff3-merge '(A B C) '() '(A B C))
                (list (list (conflict-instruction 0 3 0 0 0 3))
                      (list (conflict-block '#(A B C) 0 #() 0 #(A B C) 0))
                      (list (ok-block '#(A B C)))))

  (check-equal? (test-diff3-merge '(A B C) '() '(A D C))
                (list (list (conflict-instruction 0 3 0 0 0 3))
                      (list (conflict-block '#(A B C) 0 #() 0 #(A D C) 0))))

  (check-equal? (test-diff3-merge '(AA a b c ZZ new 00 a a M     99)
                                  '(AA       ZZ     00     M     99)
                                  '(AA a d c ZZ     11     M z z 99))
                (list (list (copy-instruction 1 0 1)
                            (conflict-instruction 1 3 1 0 1 3)
                            (copy-instruction 1 1 1)
                            (conflict-instruction 5 4 2 1 5 1)
                            (copy-instruction 1 3 1)
                            (copy-instruction 2 7 2)
                            (copy-instruction 1 4 1))
                      (list (ok-block '#(AA))
                            (conflict-block '#(a b c) 1 '#() 1 '#(a d c) 1)
                            (ok-block '#(ZZ))
                            (conflict-block '#(new 00 a a) 5 '#(00) 2 '#(11) 5)
                            (ok-block '#(M z z 99)))))

  (check-equal? (test-diff3-merge '(0 1 2 A   X DD op BB     Y E)
                                  '(0 1 2 A   X DD        C  Y E)
                                  '(0 1 2 A       AA      C  Y E))
                (list (list (copy-instruction 1 0 4)
                            (conflict-instruction 4 4 4 3 4 2)
                            (copy-instruction 1 7 2))
                      (list (ok-block '#(0 1 2 A))
                            (conflict-block '#(X DD op BB) 4 '#(X DD C) 4 '#(AA C) 4)
                            (ok-block '#(Y E)))))

  (check-equal? (test-diff3-merge '(0 1 2 A op BB X DD C Y E)
                                  '(0 1 2 A       X DD C Y E)
                                  '(0 1 2 A         AA C Y E))
                (list (list (copy-instruction 1 0 4)
                            (conflict-instruction 4 4 4 2 4 1)
                            (copy-instruction 1 6 3))
                      (list (ok-block '#(0 1 2 A))
                            (conflict-block '#(op BB X DD) 4 '#(X DD) 4 '#(AA) 4)
                            (ok-block '#(C Y E)))))

  (check-equal? (test-diff3-merge "The red brown fox jumped over the rolling log"
                                  "The quick brown fox jumps over the lazy dog"
                                  "The brown spotted fox leaps over the rolling log"
                                  (lambda (v) (list->string (vector->list v))))
                (list (list (copy-instruction 1 0 4)
                            (conflict-instruction 4 4 4 6 4 0)
                            (copy-instruction 1 10 6)
                            (copy-instruction 2 10 8)
                            (copy-instruction 1 16 4)
                            (copy-instruction 2 22 3)
                            (copy-instruction 1 23 1) ;; first problem output - 1 20 4
                            (copy-instruction 0 22 2)
                            (copy-instruction 1 25 10)
                            (conflict-instruction 34 2 35 0 37 2)
                            (copy-instruction 1 35 1)
                            (conflict-instruction 37 4 36 3 40 4)
                            (copy-instruction 1 39 1)
                            (conflict-instruction 42 1 40 1 45 1)
                            (copy-instruction 1 41 2))
                      (list (ok-block "The ")
                            (conflict-block "red " 4 "quick " 4 "" 4)
                            (ok-block "brown spotted fox leaped over the ")
                            (conflict-block "ro" 34 "" 35 "ro" 37)
                            (ok-block "l")
                            (conflict-block "ling" 37 "azy" 36 "ling" 40)
                            (ok-block " ")
                            (conflict-block "l" 42 "d" 40 "l" 45)
                            (ok-block "og"))
                      (list (ok-block "The ")
                            (conflict-block "red " 4 "quick " 4 "" 4)
                            (ok-block "brown spotted fox leaped over the rolling log"))))
  )
