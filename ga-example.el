;;; ga-example.el --- Simple examples to go with ga.el
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
;; Some trivial examples.

;;; History:
;; 

(require 'cl)
(require 'ga)

;;; Code:

(defun ga-example-onemax-fitness (genotype)
  "The fitness function for the one-max problem counts ones in GENOTYPE."
  (loop for codon in genotype sum codon))

(defun ga-example-onemax ()
  "Search for a solution to the one-max problem."
  ;; The goal is to find geotypes which have the largest number of
  ;; bits lit.  The best solution is (1 1 1 1 1 1 1 1), the worst
  ;; solution is (0 0 0 0 0 0 0 0), and we have to figure that out
  ;; using GA.
  (loop repeat 10
        for population = (ga-evaluate-population
                          (ga-make-population 10 0 2 8 8)
                          #'ga-example-onemax-fitness)
        then (ga-evaluate-population
              (ga-tournament population
                             #'ga-one-point-crossover
                             0.8
                             (ga-make-single-point-mutator 0 2)
                             0.2)
              #'ga-test-onemax-fitness)
        do (message "best = %S fitness = %s"
                    (ga-individual-genotype (first population))
                    (ga-individual-fitness (first population)))))

(provide 'ga-example)

;;; ga-example.el ends here
