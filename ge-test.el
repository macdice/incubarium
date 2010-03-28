;;; ge-test.el --- Unit tests for ge.el
;; Copyright (c) 2010 Thomas Munro <munro@ip9.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 

;;; History:
;; 

(require 'cl)
(require 'ge)

;;; Code:

(defun ge-test-1 ()
  "A trivial warm-up test."
  (let ((grammar '((<expression> = <arithmetic> | <constant>)
                   (<arithmetic> = (<op> <expression> <expression>))
                   (<op> = + | - | % | *)
                   (<constant> = 1 | 2 | 3)))
        (genotype '(0 0 1 1 1 1)))
    (assert (equal (ge-genotype->phenotype genotype grammar)
                   '(- 2 1)))))

(defun ge-test-2 ()
  "A simple test based on an example in O'Neill06."
  ;; Example from 4.1.3 of [2] We would normally use prefix operators
  ;; but this example uses infix operators, and we need to do the same
  ;; if we want the genotype from the example to generate the same
  ;; result, so the resulting phenotype is not as Lispy as one would
  ;; hope.  Our handling of <int-const> and <real-const> is a bit funny
  ;; because we don't support production rules like 0.<int-const>.  We
  ;; skip the first rule from the book <S> = <trading-rule> because in
  ;; the book that rule doesn't consume a codon, which seems like a
  ;; rather pointless special case (we always start with first rule
  ;; instead of having an explicit <S> rule).
  (let ((grammar '((<trading-rule> = (if <signal> then <trade> else <trade>))
                   (<signal>       = (<value> <rel-op> <var>)
                                   | (<signal> and <signal>)
                                   | (<signal> or <signal>))
                   (<value>        = <int-const> | <real-const>)
                   (<rel-op>       = <= | >=)
                   (<trade>        = buy | sell | do-nothing)
                   (<int-const>    = ((10 * <int-const>) + <int-const>)
                                   | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9)
                   (<real-const>   = (<int-const> / 10.0))
                   (<var>          = var0 | var1 | var2 | var3 | var4
                                   | var5 | var6 | var7 | var8 | var9)))
        (genotype '(42 22 6 104 70 31 13 4 25 9 3 86 44 48 3 27 4 111 56 2)))
    (assert (equal (ge-genotype->phenotype genotype grammar)
                   '(if ((((10 * 1) + 3) <= var5) and ((4 / 10.0) <= var3))
                        then buy
                        else sell)))))

(provide 'ge-test)

;;; ge-test.el ends here
