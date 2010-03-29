;; On Emacs before 23, you could run this at the command line with
;; something like this (assuming you are sitting in the incubarium
;; directory):
;; 
;;   emacs --batch -nw --eval '(push "." load-path)' \
;;         --load ge-example.el -f ge-example-helloworld
;;
;; On Emacs 23 and later, you could run this with something like:
;;
;;   emacs --script ge-example.el -f ge-example-helloworld TODO TODO

(require 'cl)
(require 'ga)
(require 'ge)

;; The hello world example based on the GEVA tutorial
;; TODO does not produce correct results yet!

(defvar ge-example-helloworld-grammar
  '((<prog>      = <func> | (progn <func> <prog>))
    (<func>      = (write-char <letter>))
    (<letter>    = <vowel> | <consonant> | <space>)
    (<space>     = ?\ )
    (<vowel>     = ?a | ?e | ?i | ?o | ?u)
    (<consonant> = ?q | ?w | ?r | ?t | ?y | ?p | ?s | ?d | ?f | ?g 
                 | ?h | ?j | ?k | ?l | ?z | ?x | ?c | ?v | ?b | ?n | ?m))
  "The grammar for the Hello World example.")

(defun ge-example-helloworld-fitness (genotype)
  "Compute the fitness of GENOTYPE.
This algorithm should match that the one from GEVA's WorldWriter.java."
  (let* ((target "Hello World")
         (phenotype (ge-genotype->phenotype genotype 
                                            ge-example-helloworld-grammar))
         (output (with-output-to-string (eval phenotype))))
    ;(message "genotype = %S" genotype)
    ;(message "phenotype = %S" phenotype)
    ;(message "generated %s" output)
    (- (+ (abs (- (length target) (length output)))
          (loop for target-char in target
                for output-char in output
                sum (if (= target-char output-char) 0 1))))))

(defun ge-example-helloworld ()
  "Run the Hello World example."
  ;; TODO not yet giving good results, I think the fitness numbers
  ;; need some conversion (inversion?)
  (loop repeat 10
        for population = (ga-evaluate-population
                          (ga-make-population 100 0 256 30 30)
                          #'ge-example-helloworld-fitness)
        then (ga-evaluate-population
              (ga-tournament population
                             #'ga-one-point-crossover
                             0.7
                             (ga-make-single-point-mutator 0 256)
                             0.02)
              #'ge-example-helloworld-fitness)
        do (message "best = %S fitness = %s"
                    (ga-individual-genotype (first population))
                    (ga-individual-fitness (first population)))))
