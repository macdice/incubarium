;;; ga.el --- Toy genetic algorithm library
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
;; Educational excercises to learn about basic genetic algorithm
;; programming.
;;
;; REFERENCES
;;
;; * http://en.wikipedia.org/wiki/Genetic_algorithm
;; * http://en.wikipedia.org/wiki/Crossover_(genetic_algorithm)

;;; History:
;; 

(require 'cl)

;;; Code:

;;; OPERATIONS ON GENOTYPES

(defun ga-make-random-genotype (min-value max-value min-size max-size)
  "Generate a new random genotype (aka chromosomes).
The genotype is a list of values chosen randomly from the
interval [MIN-VALUE, MAX-VALUE), of floating point type if either
of those values is a floating point number, and integer type
otherwise.  The size of the list is randomly chosen from the
inverval [MIN-SIZE, MAX-SIZE)."
  (let ((value-range (- max-value min-value))
        (size (if (>= min-size max-size)
                  min-size
                  (+ min-size (random (- max-size min-size))))))
    (if (or (floatp min-value) (floatp max-value))
        (loop repeat size collect
              (+ min-value (* value-range (/ (random 1000000) 1000000.0))))
        (loop repeat size collect (+ min-value (random value-range))))))

(defun ga-one-point-crossover (male female)
  "Return two new genotypes which result from combining MALE and FEMALE.
The two parent genotypes are split at a single random point, as
illustrated:

               +---------- randomly chosen point for split
               |
  Male:    (1 0|1 1 1)
  Female:  (0 0|0 1 1)
               |
  Child 1: (1 0|0 1 1) --- head of male, tail of female
  Child 2: (0 0|1 1 1) --- head of female, tail of male
               |

This operation works best if MALE and FEMALE are of the same
length, but it can handle geotypes of differing lengths.  The two
children are returned as a multiple values."
  (let ((point (random (max (length male) (length female)))))
    (values (append (ga-take male point) (ga-drop female point))
            (append (ga-take female point) (ga-drop male point)))))

(defun ga-cut-and-splice (male female)
  "Return two genotypes which result from combining MALE and FEMALE.
The two parent genotypes are split at two randomly chosen points,
as illustrated:

               +---------- randomly chosen point for split
               |
  Male:    (1 0|1 1 1)
  Female:  (0 0 0|1 1)
                 |
                 +-------- randomly chosen point for split

  Child 1: (1 0|1 1)   --- head of male, tail of female
  Child 2: (0 0 0|1 1 1) - head of female, tail of male

It does not matter if the two chromosomes are of different
lengths, and the child chromosomes can be of any length from zero
to the combined length of MALE and FEMALE."
  (let ((male-point (random (length male)))
        (female-point (random (length female))))
    (values (append (ga-take male male-point) (ga-drop female female-point))
            (append (ga-take female female-point) (ga-drop male male-point)))))

;; TODO
;; * ga-two-point-crossover
;; * ga-uniform-crossover
;; * ga-half-uniform-crossover
;; * ga-ordered-crossover (for the travelling salesman problem!)
;; * ga-edge-recombination-crossover
;; * ga-partially-mapped-crossover

(defun ga-make-single-point-mutator (min-value max-value)
  "Make a function to randomise one integer to [MIN-VALUE, MAX-VALUE)."
  (lexical-let ((min-value min-value)
                (max-value max-value))
    (lambda (genotype)
      (let ((point (random (length genotype))))
        (append (ga-take genotype point)
                (list (+ min-value (random (- max-value min-value))))
                (ga-drop genotype (1+ point)))))))

;; TODO
;; * more kinds of mutation, support for real values etc
;; * element swapping, ...

;; OPERATIONS ON INDIVIDUALS (GENOTYPE/FITNESS PAIRS)

(defun ga-make-individual (genotype &optional fitness)
  "Construct an individual from GENOTYPE and FITNESS."
  ;; we just use pairs to hold genotype/fitness for now
  ;; TODO is there any point in defining a struct for this?
  ;; I like the simplicity of the pair-based approach
  (cons genotype fitness))

(defun ga-individual-genotype (individual)
  "Return the genotype component of INDIVIDUAL."
  (car individual))

(defun ga-individual-fitness (individual)
  "Return the fitness component of INDIVIDUAL."
  (cdr individual))

;;; OPERATIONS ON POPULATIONS OF INDIVIDUALS

(defun ga-make-population (size min-value max-value min-size max-size)
  "Generate an initial population made from random geotypes.
The result is a list of SIZE lists, each of which is randomly
created with values from the interval [MIN-VALUE, MAX-VALUE), and
sizes from the interval [MIN-SIZE, MAX-SIZE)."
  (loop repeat size collect
        (ga-make-individual
         (ga-make-random-genotype min-value max-value min-size max-size))))

(defun ga-evaluate-population (population fitness-function)
  "Evaluate POPULATION assigning a fitnesses using FITNESS-FUNCTION.
Population is a list of individuals ie (program . fitness) pairs.
The result is a new population list, sorted by fitness.
Individuals with NIL for fitness are evaluated.  Individuals that
already have a fitness value are not reassessed; this avoids
unnecessary reevaluation in cases where individuals are copied
into the next generation verbatim."
  (ga-non-destructive-sort
   (mapcar (lambda (individual)
             (if (null (ga-individual-fitness individual))
                 (ga-make-individual 
                  (ga-individual-genotype individual)
                  (funcall fitness-function 
                           (ga-individual-genotype individual)))
                 individual))
           population)
   (lambda (left right)
     (< (ga-individual-fitness left) (ga-individual-fitness right)))))

(defun ga-tournament (population 
                      crossover-function
                      crossover-probability
                      mutation-function
                      mutation-probability)
  "Use tournament selection to select and breed a new generation.
Random pairs of individuals are selected from POPULATION.
CROSSOVER-FUNCTION must take two individual genotypes and return
two children (by multiple value return).  CROSSOVER-PROBABILITY
is the proportion of the resulting population that will be
created by crossover.  MUTATION-FUNCTION should be a function
taking one genotype, and return a mutated genotype.
MUTATION-PROBABILITY is the proportion of the resulting
population that will be created by mutating existing individuals.
The new population will have the same number of individuals as
the given population, and if there are not enough created by
crossover or mutation, copies of winning individuals will be used
to make up the remainder."
  (when (> (+ crossover-probability mutation-probability) 1.0)
    (error "incompatible crossover-probability and mutation-probability"))
  (let* ((vec (vconcat population))
         (size (length vec))
         (crossovers (ga-quantise (* size crossover-probability) 2))
         (mutations (max (- size crossovers) 
                         (floor (* size mutation-probability))))
         (copies (- size crossovers mutations)))
    (labels ((winner ()
               (let ((individual-1 (aref vec (random size)))
                     (individual-2 (aref vec (random size))))
                 (if (> (ga-individual-fitness individual-1)
                        (ga-individual-fitness individual-2))
                     individual-1
                     individual-2))))
      (append (loop repeat crossovers append
                      (multiple-value-bind (a b)
                          (funcall crossover-function 
                                   (ga-individual-genotype (winner))
                                   (ga-individual-genotype (winner)))
                        (list (ga-make-individual a)
                              (ga-make-individual b))))
              (loop repeat mutations collect
                    (ga-make-individual 
                     (funcall mutation-function 
                              (ga-individual-genotype (winner)))))
              (loop repeat copies collect (winner))))))

;; TODO ga-roulette, ga-roulette-tournament, ... ?

;;; UTILITIES

(defun ga-quantise (x step)
  "Round X down to the nearest multiple of STEP."
  (* (floor (/ x step)) step))

(defun ga-take (list n)
  "Take from LIST only the first N elements.  From SRFI-1."
  (if (zerop n) nil (cons (car list) (ga-take (cdr list) (- n 1)))))

(defun ga-drop (list n)
  "Take from LIST the tail after the first N elements.  From SRFI-1."
  (if (zerop n) list (ga-drop (cdr list) (- n 1))))

(defun ga-non-destructive-sort (list predicate)
  "Return a new list with the elements of LIST sorted using PREDICATE."
  ;; this function is here because I hate the fact that Emacs Lisp's
  ;; SORT (just like that of Common Lisp, MacLisp and probably earlier
  ;; dialects) is destructive -- this seems to me to be wanton
  ;; destruction up with which I will not put; there should have been
  ;; a SORT and an NSORT (cf SRFI-95 SORT and SORT!, although the Nxxx
  ;; functions don't promise to mutate their input to achieve their
  ;; goal like the xxx! functions, they merely have the option of
  ;; doing so... an interesting difference), and if I ever succeed in
  ;; building a time machine (easy) and getting into MIT (hard) I will
  ;; make my opinion known at the appropriate time to sort this issue
  ;; out once and for all
  (let ((temporary (copy-seq list)))
    (sort temporary predicate)))

(provide 'ga)

;;; ga.el ends here
