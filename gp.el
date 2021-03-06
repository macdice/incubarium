;;; gp.el --- Toy genetic programming library
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
;; A toy GP system.  The algorithms here are purely function (ie they
;; do not use assignment).  Maybe that makes a couple of things a
;; little bit harder to read, I haven't decided (this is a legacy of
;; an earlier version that ran on Termite Scheme, a side-effect free
;; distributed Lisp).
;;
;; This is a set of GP-specific functions which are designed to work
;; with the support for selection and breeding provided in ga.el.
;; Genetic programming is a special case of the genetic algorithm
;; where genotypes are identical to phenotypes, that is, the genotypes
;; are the program trees we want to produce.  We need to replace a
;; couple of parts of ga.el to achieve this:
;;
;; * instead of ga-make-random-genotype, we have
;;   gp-make-random-program 
;;
;; * instead of ga-make-population, we have gp-grow-population,
;;   gp-full-population and gp-ramped-population which create
;;   populations using the above
;;
;; * instead of the various ga-XXX-crossover functions we have
;;   gp-tree-random-recombine
;;
;; * the IDENTITY function with a zero probability should be used for
;;   mutation (the literature suggests it doesn't help much with GP,
;;   but it could of course be easily done)
;;
;; Otherwise we can use the standard GA-TOURNAMENT, GA-ROULETTE etc
;; functions for genetic programming.
;;
;; REFERENCES
;; - http://en.wikipedia.org/wiki/Genetic_programming
;; - Genetic Programming, John Koza, 1992

;;; History:
;; 
;; 2005 - wrote this code in Scheme, the sweetest Lisp of them all
;;
;; 2008 - rewrote it in Common Lisp, the fastest Lisp (SBCL wins all
;;        my genetic programming drag-races)
;;
;; 2010 - rewrote it in Emacs Lisp for a laugh because it is an old
;;        faithful friend, it's the only Lisp that I can truly depend
;;        on in this mean and nasty world, even though it's as slow as
;;        a wet week and it's completely inappropriate for
;;        compute-bound work like this

;;; Code:

(require 'cl)

;;; OPERATIONS ON PROGRAM TREES

(defun gp-make-random-program (functions terminals min-depth max-depth)
  "Generate a random program.
FUNCTIONS should be a list of dotted pairs of symbol and arity,
for example ((add . 2) (print . 1)).  TERMINALS is a list of
terminal values.  Terminals can be symbols, numbers or lists of
the form (random min max) which generate a random number in the
interval [min, max).  The tree will be of a size between
MIN-DEPTH and MAX-DEPTH (inclusive)."
  (labels ((iter (depth)
             (if (and (< depth max-depth)
                      (or (< depth min-depth) 
                          (= depth max-depth)
                          (zerop (random (- max-depth depth)))))
                 ;; generate a function (ie grow bigger)
                 (let* ((pair (pick-one functions))
                        (function (car pair))
                        (num-args (cdr pair))
                        (arguments (loop repeat num-args
                                      collect (iter (+ 1 depth)))))
                   (cons function arguments))
                 ;; generate a terminal (ie stop growing)
                 (gp-pick-terminal terminals))))
    (iter 0)))

(defun gp-tree-size (tree)
  "Report the number of nodes in TREE."
  (cond ((null tree) 0)
        ((consp tree) (+ (gp-tree-size (car tree)) (gp-tree-size (cdr tree))))
        (t 1)))

(defun gp-tree-ref (tree index)
  "Return the node of TREE at position INDEX, depth first numbering."
  (labels ((walk (node count)
             (cond ((null node) nil)
                   ((= index count)
                    (car node))
                   ((consp (car node))
                    (let ((result (walk (cdar node) (+ count 1))))
                      (if (null result)
                          (walk (cdr node)
                                (+ count (gp-tree-size (car node))))
                          result)))
                   (t
                    (walk (cdr node) (+ count 1))))))
    (walk (list tree) 0)))

(defun gp-tree-combine (host index guest)
  "Return a new tree made from HOST, with node INDEX replaced by GUEST."
  ;; this would probably be simpler to understand if I weren't insisting
  ;; on doing it in purely functional style; it's a phase I was going
  ;; through
  (labels ((copy (node count)
             (cond ((null node) nil)
                   ((= index count)
                    (cons guest
                          (copy (cdr node)
                                (+ count (gp-tree-size (car node))))))
                   ((consp (car node))
                    (cons (copy (car node) count)
                          (copy (cdr node)
                                (+ count (gp-tree-size (car node))))))
                   (t
                    (cons (car node)
                          (copy (cdr node) (+ count 1)))))))
    (car (copy (list host) 0))))

(defun gp-tree-random-recombine (male female)
  "Return a new tree generated by randomly combining trees MALE and FEMALE."
  (let* ((male-index    (random (tree-size male)))
         (female-index  (random (tree-size female)))
         (male-fragment (gp-tree-ref male male-index)))
    (gp-tree-combine female female-index male-fragment)))

;;; OPERATIONS ON POPULATIONS

(defun gp-grow-population (size functions terminals min-depth max-depth)
  "Generate a population of random individuals using the 'grow' method.
This means that there are individuals with a range of different
shapes and sizes.  See Koza92 6.2 p92.  Koza's system uses a
minimum depth of 2, whereas we have a parameter."
  (loop repeat size 
        collect
        (gp-make-individual
         (gp-make-random-program functions terminals min-depth max-depth))))

(defun gp-full-population (size functions terminals depth)
  "Generate a population of random individuals using the 'full' method.
This means that individuals have the full maximum size on all
branches of initial programs.  See Koza92 6.2 p92."
  (gp-grow-population size functions terminals depth depth))

(defun gp-ramped-population (size functions terminals min-depth max-depth)
  "Generate a population using the 'ramped half-and-half' method.
This means that half of the individuals are generated using
'grow' and the rest using 'full' method, for a range of depths
between MIN-DEPTH and MAX-DEPTH, the idea being that we don't
really know how to choose a size or whether to use grow or full,
so we try everything.  See Koza92 6.2 p92."
  (loop repeat size
        for count from 0
        as depth = (+ min-depth (% (/ count 2) (- max-depth min-depth)))
        collect
        (gp-make-individual
         (case (% count 2)
           ((0) (gp-make-random-program functions terminals min-depth depth))
           ((1) (gp-make-random-program functions terminals depth depth))))))

;;; UTILITIES

(defun gp-pick-one (list)
  "Return a randomly selected item from LIST."
  (elt list (random (length list))))

(defun gp-pick-terminal (terminals)
  "Randomly pick a terminal from TERMINALS, generating any random values.
Terminals can be numbers, symbols, or lists of the form (random
<min> <max>) which generate a random number in the interval [min,
max).  The random number will be an integer or a float depending
on the type of the min and max constants provided."
  (let ((terminal (pick-one terminals)))
    (cond ((and (consp terminal)
                (eq (first terminal) 'random))
           (unless (and (= (length terminal) 3)
                        (numberp (second terminal))
                        (numberp (third terminal)))
             (error "bad syntax for terminal: %S" terminal)) 
           (let ((min (second terminal))
                 (max (third terminal)))
             (if (or (floatp min) (floatp max))
                 (+ min
                    (* (/ (random 1000000) 1000000.0)
                       (- max min)))
               (+ min (random (- max min))))))
          ((or (numberp terminal) (symbolp terminal))
           terminal)
          (t
           (error "bad syntax for terminal: %S" terminal)))))

;;; TESTS

(assert (equal (gp-tree-ref '(+ (* a b) c) 0) '(+ (* a b) c)))
(assert (equal (gp-tree-ref '(+ (* a b) c) 1) '(* a b)))
(assert (equal (gp-tree-ref '(+ (* a b) c) 2) 'a))
(assert (equal (gp-tree-ref '(+ (* a b) c) 3) 'b))
(assert (equal (gp-tree-ref '(+ (* a b) c) 4) 'c))
(assert (equal (gp-tree-ref '(+ (* a b) c) 5) nil))

(assert (equal (gp-evaluate-population '((x . nil) (y . 42))
                                       (lambda (program) 1))
               '((x . 1) (y . 42))))

(provide 'gp)

;;; gp.el ends here
