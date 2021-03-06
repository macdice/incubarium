;;; ge.el --- A toy implementation of grammatical evolution
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
;; Grammatical evolution[1] is based on standard GA techniques:
;; genotypes are strings of numbers, and the usual GA operations such
;; as crossover, mutation, and tournament selection are used.  It is
;; similar to GP in that it aims to produce a program that solves some
;; problem, but whereas GP evolves program trees directly, GE evolves
;; genotypes which are then used to produce phenotypes which are then
;; tested for fitness.  To support GE, we therefore need our existing
;; GA tools from ga.el, a grammar designed for our specific problem
;; domain, and a new genotype->phenotype operation which is defined
;; here in ge.el.
;;
;; GRAMMARS
;;
;; Grammars are lists constisting of at least one production rule.
;; Production rules are lists of one of the following forms:
;; 
;;   (<NAME> = VALUE)                 ;; expands to a value
;;   (<NAME> = (VALUE ...)            ;; expands to a form
;;   (<NAME> = VALUE | VALUE ...)     ;; expands to a choice
;;   (<NAME> = random min max)        ;; expands to a number in [min, max)
;;
;; In all of the above cases, VALUE can be a constant number, or the
;; name of another production rule (indicated by a symbol enclosed in
;; angle-brackets).
;;
;; TODO 
;;
;; * support the Prune operator described in section 5.2 of [1]
;;
;; RESOURCES
;;
;; * http://www.grammaticalevolution.org/eurogp98.ps [1]
;;
;; * Biologically Inspired Algorithms for Financial Modelling,
;;   Anthony Brabazon and Michael O'Neill, Springer Verlang, 2006 [2]
;;
;; * http://www.grammatical-evolution.org/
;;
;; * http://en.wikipedia.org/wiki/Grammatical_evolution

;;; History:
;; 

(require 'cl)

;;; Code:

(defun ge-non-terminal-p (symbol)
  "Test if SYMBOL is a non-terminal, that is, has a name liked <X>."
  (let ((name (symbol-name symbol)))
    (and (= (elt name 0) ?<)
         (= (elt name (- (length name) 1)) ?>))))

(defun ge-expand-choice (clause codon allow-growth-p)
  "Given CLAUSE of options like (x | y | z ...), map CODON to an option.
If ALLOW-GROWTH-P is true, then allow choices which generate a
form which expands to a level output; otherwise, allow only atoms, to prevent
further growth (unless all possible expansions are grown forms)."
  (let ((choices (remq '| clause)))
    (if (or allow-growth-p (every #'consp choices))
        (nth (% codon (length choices)) choices)
        (let ((atoms (ge-filter #'atom choices)))
          (nth (% codon (length atoms)) atoms)))))

(defun ge-expand-random (clause)
  "Given CLAUSE like (random <min> <max>), pick a number in [min, max)."
  (unless (and (= (length clause) 3)
               (numberp (second clause))
               (numberp (third clause)))
    (error "Syntax error: %S" clause))
  (let ((min (second rhs))
        (max (third rhs)))
    (if (or (floatp min) (floatp max))
        (+ min (* (- max min) (/ (random 1000000) 1000000.0)))
        (+ min (random (- max min))))))
        
(defun* ge-genotype->phenotype (genotype grammar &optional (grow-depth 10))
  "Convert GENOTYPE into a phenotype using GRAMMAR.
When branches of the phenotype program tree reaches depth
GROW-DEPTH, the algorithm will begin to skip options that expand
to a new level of code depth whenever possible.  This imposes a
total limit on the size of the generated program trees."
  ;; I would like to write a pure functional version of this, but I ran
  ;; out of steam while trying to implement the wrapping codon stream;
  ;; I think I can see how to do it but it's confusing
  (let ((codons genotype))
    (labels ((render (depth template)
               (when (null codons)          ;; wrap
                 (setq codons genotype))
               (cond ((and (symbolp template)
                           (ge-non-terminal-p template))
                      (let* ((rule (assq template grammar))
                             (rhs (cddr rule))
                             (codon (pop codons)))
                        (unless rule
                          (error "Unknown non-terminal %s" template))
                        (unless (eq (second rule) '=)
                          (error "Syntax error: %S" rule))
                        (render depth
                                (cond 
                                 ((memq '| rhs)
                                  (ge-expand-choice rhs
                                                    codon
                                                    (< depth grow-depth)))
                                 ((eq (first rhs) 'random)
                                  (ge-expand-random rhs))
                                 ((= (length rhs) 1)
                                  (car rhs))
                                 (t
                                  (error "Syntax error: %S" rhs))))))
                     ((consp template)
                      (loop for element in template
                            collect
                            (render (1+ depth) element)))
                     (t template))))
      (render 0 (first (first grammar))))))

;;; UTILITIES

(defun ge-filter (predicate list)
  "Return a list of elements satisfying PREDICATE from LIST.  Like SRFI-1."
  (loop for x in list if (funcall predicate x) collect x))

(provide 'ge)

;;; ge.el ends here