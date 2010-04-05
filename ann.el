;;; ann.el --- A toy neural network library
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
;; This is loosely based on/ported from public domain Python code by
;; Neil Schemenauer[1].  The Lisp code is probably not as readable as
;; the Python original, I'm afraid, mainly down to the lack of concise
;; array subscript syntax, but I did try to make use of the mighty
;; LOOP's ability to walk data structures in parallel rather that
;; using AREF to explicitly get values from vectors, wherever possible
;; (ie when reading in the appropriate order).
;;
;; The code uses Scheme-style exclamation marks to indicate mutation
;; of function arguments, because today I am feeling schematic.  
;;
;; It probably goes without saying that the equivalent code runs
;; thousands of times faster in SBCL.
;;
;; Some relevant web pages:
;; 
;; * http://en.wikipedia.org/wiki/Backpropagation
;; * http://www.tek271.com/?about=docs/neuralNet/IntoToNeuralNets.html
;; * http://galaxy.agh.edu.pl/~vlsi/AI/backp_t_en/backprop.html
;; * http://arctrix.com/nas/python/bpnn.py [1]
;; * http://en.wikipedia.org/wiki/Logistic_function

;;; History:
;; 

(require 'cl)

;;; Code:

;;; NUMERIC UTILITIES

(defun ann-random (a b)
  "Return a random floating point number in the range [A, B)."
  (+ a (* (- b a) (/ (random 1000000) 1000000.0))))

(defun ann-logistic (x)
  "The logistic function, mapping X to the range 0, 1 (a sigmoid function)."
  (/ 1 (+ 1 (exp (- x)))))

(defun ann-logistic-derivative (x)
  "The derivative of the logistic function, mapping X to the range 0, 1."
  (* (ann-logistic x) (- 1 (ann-logistic x))))

(defun ann-tanh (x)
  "The hyperbolic tangent function for X (another sigmoid function)."
  (/ (- (exp x) (exp (- x)))
     (+ (exp x) (exp (- x)))))

(defun ann-tanh-derivative (y)
  "The derivative of TANH in terms of Y."
  (- 1.0 (* y y)))

;;; OPERATIONS ON MATRICES

(defun* ann-make-matrix (rows columns &optional (fill 0.0))
  "Construct a 2D matrix of size ROWS x COLUMNS, filled with the value FILL."
  (vconcat (loop repeat rows collect (make-vector columns fill))))

(defun ann-matrix-set! (matrix row column value)
  "Set cell of MATRIX at (ROW, COLUMN) to VALUE."
  (setf (aref (aref matrix row) column) value))

(defun ann-matrix-get (matrix row column)
  "Get value of MATRIX at ROW, COLUMN."
  (aref (aref matrix row) column))

;;; OPERATIONS ON NUERAL NETWORKS

(defstruct ann-network
  "A neural network."
  (input-count)
  (hidden-count)
  (output-count)
  (input-activation)
  (hidden-activation)
  (output-activation)
  (input-weights)
  (output-weights)
  (input-change)
  (output-change))

(defun ann-create (input-count hidden-count output-count)
  "Return a new artificial neural network.
The input layer will have INPUT-COUNT input nodes, HIDDEN-COUNT
nodes in the hidden layer, and OUTPUT-COUNT output nodes.  The
weights are randomly initialised."
  (let* ((input-weights (ann-make-matrix (1+ input-count) hidden-count))
         (output-weights (ann-make-matrix hidden-count output-count))
         (result (make-ann-network
                  :input-count (1+ input-count)
                  :hidden-count hidden-count
                  :output-count output-count
                  :input-activation (make-vector (1+ input-count) 1.0)
                  :hidden-activation (make-vector hidden-count 1.0)
                  :output-activation (make-vector output-count 1.0)
                  :input-weights input-weights
                  :output-weights output-weights
                  :input-change (ann-make-matrix (1+ input-count) hidden-count)
                  :output-change (ann-make-matrix hidden-count output-count))))
    ;; initialise weights to random values
    (loop for i from 0 below input-count do
          (loop for j from 0 below hidden-count do
                (ann-matrix-set! input-weights i j (ann-random -0.2 0.2))))
    (loop for j from 0 below hidden-count do
          (loop for k from 0 below output-count do
                (ann-matrix-set! output-weights j k (ann-random -2.0 2.0))))
    result))

(defun ann-update! (network values)
  "Update the input activation nodes of NETWORK with VALUES.
The the output activation node vector is returned."
  (unless (= (length values) (1- (ann-network-input-count network)))
    (error "Wrong number of inputs"))
  ;; set input activations
  (loop for value across values
        for i from 0 do
        (setf (aref (ann-network-input-activation network) i) value))
  ;; compute hidden activations
  (let ((input-weights    (ann-network-input-weights network))
        (input-activation  (ann-network-input-activation network))
        (hidden-activation (ann-network-hidden-activation network)))
    (loop for j from 0 below (ann-network-hidden-count network) do
          (setf (aref hidden-activation j)
                (ann-tanh
                 (loop for input-act across input-activation
                       for i from 0
                       sum (* input-act
                              (ann-matrix-get input-weights i j)))))))
  ;; compute output activations
  (let ((output-weights    (ann-network-output-weights network))
        (hidden-activation (ann-network-hidden-activation network))
        (output-activation (ann-network-output-activation network)))
    (loop for k from 0 below (ann-network-output-count network) do
          (setf (aref output-activation k)
                (ann-tanh
                 (loop for j from 0 below (ann-network-hidden-count network)
                       sum (* (aref hidden-activation j)
                              (ann-matrix-get output-weights j k)))))))
  (ann-network-output-activation network))

(defun ann-back-propagate! (network targets n m)
  "Adjust the weights of NETWORK to reduce error for expected result TARGETS.
The learning rate should be provided in N, and the momentum in M.
The error is returned."
  (unless (= (length targets) (ann-network-output-count network))
    (error "Wrong number of target values"))
  (let ((output-activation (ann-network-output-activation network))
        (output-weights    (ann-network-output-weights network))
        (output-change     (ann-network-output-change network))
        (hidden-activation (ann-network-hidden-activation network))
        (input-weights     (ann-network-input-weights network))
        (input-change      (ann-network-input-change network))
        (input-activation  (ann-network-input-activation network)))
    ;; compute the output and hidden errors terms
    (let* ((output-deltas 
            (loop for target across targets
                  for output-act across output-activation
                  as error = (- target output-act)
                  collect (* (ann-tanh-derivative output-act) error)))
           (hidden-deltas 
            (loop for output-weight-row across output-weights
                  for hidden-act across hidden-activation
                  as error = (loop for output-delta in output-deltas
                                   for weight across output-weight-row
                                   sum (* output-delta weight))
                  collect (* (ann-tanh-derivative hidden-act) error))))
      ;; update output weights
      (loop for output-weight-row across output-weights
            for output-change-row across output-change
            for hidden-act across hidden-activation do
            (loop for k from 0
                  for output-delta in output-deltas
                  as change = (* output-delta hidden-act) do
                  (incf (aref output-weight-row k)
                        (+ (* n change)
                           (* m (aref output-change-row k))))
                  (setf (aref output-change-row k) change)))
      ;; update the input weights
      (loop for i from 0
            for input-weight-row across input-weights
            for input-change-row across input-change do
            (loop for j from 0 below (ann-network-hidden-count network)
                  for delta in hidden-deltas
                  for input-act across input-activation
                  as change = (* delta (aref input-activation i)) do
                  (incf (aref input-weight-row j)
                        (+ (* n change) (aref input-change-row j)))
                  (setf (aref input-change-row j) change))))
    ;; calculate and return error
    (loop for target across targets
          for output across output-activation
          sum (* 0.5 (expt (- target output) 2)))))

(defun* ann-train! (network training-data &optional 
                            (iterations 1000)
                            (n 0.5)
                            (m 0.1))
  "Train NETWORK using TRAINING-DATA.
The training data should be a list of lists containing two
vectors of numbers, representing the input values and expected
output values.  The neural network is trained ITERATIONS times,
using learning rate N and momentum M."
  ;; TODO return the error!
  (loop repeat iterations do
        (loop for (inputs targets) in training-data
              as outputs = (ann-update! network inputs)
              sum (ann-back-propagate! network targets n m))))

(provide 'ann)

;;; ann.el ends here
