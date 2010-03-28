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

(defun ge-collect-every-former (list)
  "Return every second value from LIST, starting with the first item."
  (loop for object in list
        for i from 0
        when (zerop (% i 2))
        collect object))

(defun ge-collect-every-latter (list)
  "Return every second value from LIST, starting with the second item."
  (ge-collect-every-former (cdr list)))

(defun ge-grammar-choices (list)
  "Test if LIST is a list of x | y | ... and if so return the options.
If not, return nil."
  (if (and (> (length list) 1)
           (= (% (length list) 2) 1)
           (every (lambda (object)
                    (eq object '|))
                  (ge-collect-every-latter list)))
      (ge-collect-every-former list)
      nil))

(defun ge-genotype->phenotype (genotype grammar)
  "Convert GENOTYPE into a phenotype using GRAMMAR."
  ;; I would like to write a pure functional version of this, but I ran
  ;; out of steam while trying to implement the wrapping codon stream;
  ;; I think I can see how to do it but it's confusing
  (let ((codons genotype))
    (labels ((render (template)
               (when (null codons)          ;; wrap
                 (setq codons genotype))
               (cond ((numberp template) template)
                     ((and (symbolp template)
                           (ge-non-terminal-p template))
                      (let ((rule (assq template grammar))
                            (codon (pop codons)))
                        (unless rule
                          (error "Unknown non-terminal %s" template))
                        (unless (eq (second rule) '=)
                          (error "Syntax error: %S" rule))
                        (render 
                         (let ((choices (ge-grammar-choices (cddr rule))))
                           (if choices
                               (nth (% codon (length choices)) choices)
                               (third rule))))))
                     ((symbolp template) template)
                     ((and (consp template)
                           (eq (first template) 'random))
                      ;; TODO fixme
                      (random (second template)))
                     ((consp template)
                      (loop for element in template
                            collect
                            (render element))))))
      (render (first (first grammar))))))
         
(provide 'ge)

;;; ge.el ends here
