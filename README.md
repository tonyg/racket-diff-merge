# Diff and Merge for Racket Sequences

This package provides a “Myers-Ukkonen” sequence *diff* algorithm
[1,2] and a three-way sequence *merge* algorithm that uses the diff
routine as a primitive, based on the Unix `diff3` algorithm [3].

 1. Eugene W. Myers, “An O(ND) difference algorithm and its
    variations,” Algorithmica, vol. 1, no. 1–4, pp. 251–266, Nov.
    1986.

 1. Esko Ukkonen, “Algorithms for approximate string matching,” Inf.
    Control, vol. 64, no. 1–3, pp. 100–118, Jan. 1985.

 1. Sanjeev Khanna, Keshav Kunal, and Benjamin C. Pierce, “A Formal
    Investigation of Diff3,” in Arvind and Prasad, eds., Proc. Conf.
    on Foundations of Software Technology and Theoretical Computer
    Science (FSTTCS), Dec. 2007.

The code draws on several other sources: primarily on my own
[Javascript diff/merge implementation](https://github.com/tonyg/synchrotron/blob/master/diff.js),
but also on joint work with Levente Uzonyi on a
[Smalltalk diff implementation](http://squeaksource.com/DiffMerge.html).

## Usage

Programs should `require` the `diff-merge` collection:

```racket
(require diff-merge)
```

### Diffing

Differences between sequences can be computed with `diff-indices`:

```racket
> (diff-indices "The red brown fox jumped over the rolling log"
                "The brown spotted fox leaps over the rolling log")
```

The result is a list of `difference` structs, sorted in order of their
position with respect to the longest common subsequence of the two
input sequences:

```racket
'(#s(difference 4 4 4 0)
  #s(difference 14 0 10 8)
  #s(difference 18 4 22 1)
  #s(difference 23 1 24 3))
```

A `difference` is a `prefab` structure representing a mismatch between
the old sequence (first argument to `diff-indices`) and the new
sequence (second argument):

```racket
(struct difference (old-lhs old-len new-lhs new-len) #:prefab)
```

The struct `(difference 18 4 22 1)` is interpreted as "a difference
between the sequences has been detected; the subsequence of length 4
starting at index 18 of the old sequence does not match the
subsequence of length 1 starting at index 22 of the new sequence".

Any Racket `sequence?` can be diffed:

```racket
> (diff-indices '(A B C D E) '(A X C Y E))
'(#s(difference 1 1 1 1) #s(difference 3 1 3 1))
> (diff-indices #"hello" #"goodbye")
'(#s(difference 0 4 0 1) #s(difference 5 0 2 5))
```

### Merging sequences

The procedure `diff3-merge-instructions` computes a sequence of
instructions for constructing a *merge* of two sequences that have a
common ancestor. The common ancestor sequence is customarily called
`O`; the two derived sequences are called `A` and `B`. Pass the
sequences in `A`, `O`, `B` order to the merge procedures.

```racket
> (diff3-merge-instructions
   "The red brown fox jumped over the rolling log"      ;; A
   "The quick brown fox jumps over the lazy dog"        ;; O
   "The brown spotted fox leaps over the rolling log")  ;; B
```

The result is a sequence of `copy-instruction` and
`conflict-instruction` records which describe portions of the input
sequences to copy to the output, and regions where the two new
sequences conflict with one another, respectively. The call to
`diff3-merge-instructions` above results in:

```racket
'(#s(copy-instruction 1 0 4)
  #s(conflict-instruction 4 4 4 6 4 0)
  #s(copy-instruction 1 10 6)
  #s(copy-instruction 2 10 8)
  #s(copy-instruction 1 16 4)
  #s(copy-instruction 2 22 3)
  #s(copy-instruction 1 20 4)
  #s(copy-instruction 0 22 2)
  #s(copy-instruction 1 24 11)
  #s(conflict-instruction 34 2 35 0 37 2)
  #s(copy-instruction 1 35 1)
  #s(conflict-instruction 37 4 36 3 40 4)
  #s(copy-instruction 1 39 1)
  #s(conflict-instruction 42 1 40 1 45 1)
  #s(copy-instruction 1 41 2))
```

The procedure `diff3-merge` goes a step further, following the
computed instructions to reconstruct a merged sequence of items.

```racket
> (diff3-merge "The red brown fox jumped over the rolling log"
               "The quick brown fox jumps over the lazy dog"
               "The brown spotted fox leaps over the rolling log")
```

```racket
'(#s(ok-block #(#\T #\h #\e #\space))
  #s(conflict-block #(#\r #\e #\d #\space) 4
                    #(#\q #\u #\i #\c #\k #\space) 4
                    #() 4)
  #s(ok-block #(#\b #\r #\o #\w #\n #\space . . . #\space #\t #\h #\e #\space))
  #s(conflict-block #(#\r #\o) 34
                    #() 35
                    #(#\r #\o) 37)
  #s(ok-block #(#\l))
  #s(conflict-block #(#\l #\i #\n #\g) 37
                    #(#\a #\z #\y) 36
                    #(#\l #\i #\n #\g) 40)
  #s(ok-block #(#\space))
  #s(conflict-block #(#\l) 42
                    #(#\d) 40
                    #(#\l) 45)
  #s(ok-block #(#\o #\g)))
```

By default, `diff3-merge` will construct *vectors*; supply a
`#:convert-block` argument to produce something else:

```racket
(diff3-merge #:convert-block (lambda (v) (list->string (vector->list v)))
             "The red brown fox jumped over the rolling log"
             "The quick brown fox jumps over the lazy dog"
             "The brown spotted fox leaps over the rolling log")
```

```racket
'(#s(ok-block "The ")
  #s(conflict-block "red " 4 "quick " 4 "" 4)
  #s(ok-block "brown spotted fox leaped over the ")
  #s(conflict-block "ro" 34 "" 35 "ro" 37)          ;; *
  #s(ok-block "l")
  #s(conflict-block "ling" 37 "azy" 36 "ling" 40)   ;; *
  #s(ok-block " ")
  #s(conflict-block "l" 42 "d" 40 "l" 45)           ;; *
  #s(ok-block "og"))
```

A "false conflict" is when sequences `A` and `B` make the *same
change* to `O` at the *same place*. There are three examples in the
output immediately above (marked with `*`), all resulting from the
change from `"lazy dog"` to `"rolling log"`.

In some cases, applications need to know about false conflicts. In
other cases, false conflicts are harmless, and should be treated as a
non-conflicting change. Supply `#t` as the
`#:exclude-false-conflicts?` argument to `diff3-merge` to get the
latter behaviour:

```racket
(diff3-merge #:exclude-false-conflicts? #t
             #:convert-block (lambda (v) (list->string (vector->list v)))
             "The red brown fox jumped over the rolling log"
             "The quick brown fox jumps over the lazy dog"
             "The brown spotted fox leaps over the rolling log")
```

```racket
'(#s(ok-block "The ")
  #s(conflict-block "red " 4 "quick " 4 "" 4)
  #s(ok-block "brown spotted fox leaped over the rolling log"))
```

## Licence

Copyright (C) 2011–2018 Tony Garnock-Jones <tonyg@leastfixedpoint.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program (see the files "lgpl.txt" and
"gpl.txt"). If not, see <http://www.gnu.org/licenses/>.
