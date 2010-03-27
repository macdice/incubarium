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
;; An educational excercise in basic AI hacking.
;;
;; TODO -- more crossover algorithms
;; * ga-two-point-crossover
;; * ga-uniform-crossover
;; * ga-half-uniform-crossover
;; * ga-ordered-crossover
;; * ga-edge-recombination-crossover
;; * ga-partially-mapped-crossover
;;
;; REFERENCES
;;
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
        (loop repeat size
              collect (+ min-value
                         (* value-range (/ (random 1000000) 1000000.0))))
        (loop repeat size collect (+ min-value (random value-range))))))

(defun ga-make-population (size min-value max-value min-size max-size)
  "Generate an initial population made from random geotypes.
The result is a list of SIZE lists, each of which is randomly
created with values from the interval [MIN-VALUE, MAX-VALUE), and
sizes from the interval [MIN-SIZE), MAX-SIZE)."
  (loop repeat size
        collect
        (ga-make-random-genotype min-value max-value min-size max-size)))

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
  (let* ((max-length (max (length male) (length female)))
         (point (random max-length)))
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

;;; UTILITIES

(defun ga-take (list n)
  "Take from LIST only the first N elements.  From SRFI-1."
  (if (zerop n) nil (cons (car list) (ga-take (cdr list) (- n 1)))))

(defun ga-drop (list n)
  "Take from LIST the tail after the first N elements.  From SRFI-1."
  (if (zerop n) list (ga-drop (cdr list) (- n 1))))


(provide 'ga)

;;; ga.el ends here
