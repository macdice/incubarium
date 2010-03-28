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
;; UNFINISHED -- WATCH THIS SPACE
;;
;; GRAMMARS
;;
;; Grammars are lists constisting of at least one production rule.
;; Production rules are lists of one of the following forms:
;; 
;; (<name> = value)                 ;; expands to a value
;; (<name> = (value ...))           ;; expands to a form
;; (<name> = value | value ...)     ;; expands to a choice
;;
;; In all of the above cases, value could be a constant number or ... TODO
;;
;; The plan:
;; * use GA support from ga.el, but provide tools for GE fitness functions
;;
;; This is my interpretation of what these guys are trying to say:
;; * http://www.grammatical-evolution.org/
;; * http://en.wikipedia.org/wiki/Grammatical_evolution
;; * http://www.grammaticalevolution.org/eurogp98.ps

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
  (loop for object in list
        for i from 1
        when (zerop (% i 2))
        collect object))

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
  (let ((codons genotype))
    (labels ((render (template)
               (when (null codons)          ;; wrap
                 (setq codons genotype))
               (cond ((numberp template) template)
                     ((and (symbolp template)
                           (ge-non-terminal-p template))
                      (let ((rule (assq template grammar)))
                        (unless rule
                          (error "Unknown non-terminal %s" template))
                        (unless (eq (second rule) '=)
                          (error "Syntax error: %S" rule))
                        (render 
                         (let ((choices (ge-grammar-choices (cddr rule))))
                           (if choices
                               (nth (% (pop codons) (length choices)) choices)
                             (third rule))))))
                     ((symbolp template) template)
                     ((and (consp template)
                           (eq (first template) 'random))
                      (random (second template)))
                     ((consp template)
                      (loop for element in template
                            collect
                            (render element))))))
      (render (first (first grammar))))))

(defun ge-test ()
  (let ((grammar '((<expression> = <arithmetic> | <constant>)
                   (<arithmetic> = (<op> <expression> <expression>))
                   (<op> = + | - | % | *)
                   (<constant> = 1 | 2 | 3)))
        (genotype '(0 0 1 1 1 1)))
    (assert (equal (ge-genotype->phenotype genotype grammar)
                   '(+ 2 2)))))
                                     
(provide 'ge)

;;; ge.el ends here
