;;; ann-example.el --- Examples for ann.el
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
;; You can run this with something like:
;;
;;   emacs --batch -nw --eval '(push "." load-path)' \
;;         --load ann-example.el -f ann-example-xor

;;; History:
;; 

(require 'ann)
(require 'cl)

;;; Code:

(defun ann-example-xor ()
  "Learn the exclusive or function by example."
  (let ((network (ann-create 2 2 1))
        (training-data '(([0 0] [0])
                         ([0 1] [1])
                         ([1 0] [1])
                         ([1 1] [0]))))
    ;; train it using the stimulus/reponse data
    (ann-train! network training-data)
    ;; show the results
    (loop for (input expected) in training-data do
          (message "%S -> %S"
                   input
                   (ann-update! network input)))))
    
(provide 'ann-example)

;;; ann-example.el ends here
