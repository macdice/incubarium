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

;;; OPERATIONS ON INDIVIDUALS

(defun gp-make-individual (program &optional fitness)
  "Construct an individual from PROGRAM (tree) and FITNESS."
  ;; we just use pairs to hold program/fitness for now
  ;; TODO is there any point in defining a struct for this?
  ;; I like the simplicity of the pair-based approach
  (cons program fitness))

(defun gp-individual-program (individual)
  "Return the program (tree) component of INDIVIDUAL."
  (car individual))

(defun gp-individual-fitness (individual)
  "Return the fitness component of INDIVIDUAL."
  (cdr individual))

;;; OPERATIONS ON PROGRAM TREES

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
                          (zerop (random 2)))) ;; TODO -- depths not equally likely!
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

(defun gp-ramped-population (size functions terminal min-depth max-depth)
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
           ((0) (gp-make-random-program functions terminal min-depth depth))
           ((1) (gp-make-random-program functions terminals depth depth))))))

(defun gp-evaluate-population (population fitness-function)
  "Evaluate POPULATION assigning a fitnesses using FITNESS-FUNCTION.
Population is a list of individuals ie (program . fitness) pairs.
The result is a new population list, sorted by fitness.
Individuals with NIL for fitness are evaluated.  Individuals that
already have a fitness value are not reassessed; this avoids
unnecessary reevaluation in cases where individuals are copied
into the next generation verbatim."
  (gp-non-destructive-sort
   (mapcar (lambda (individual)
             (if (null (gp-individual-fitness individual))
                 (gp-make-individual 
                  (gp-individual-program individual)
                  (funcall fitness-function 
                           (gp-individual-program individual)))
                 individual))
           population)
   (lambda (left right)
     (< (gp-individual-fitness left) (gp-individual-fitness right)))))

;;; UTILITIES

(defun gp-non-destructive-sort (list predicate)
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

(defun gp-pick-one (list)
  "Return a randomly selected item from LIST."
  (elt list (random (length list))))

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
