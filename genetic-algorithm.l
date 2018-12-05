(load "image.l")
(load "image-search.l")
(load "images.l")
(load "demos.l")

(defconstant *nr-generations* 10)
(defconstant *percent-crossover* 50)
(defconstant *percent-mutate* (- 100 *percent-crossover*))
(defconstant *mutation-chance* .5)
(defconstant *mutation-degree* 16)

(defconstant *population-size* 50)
(defconstant *sample-size* 6)
(defconstant *nr-crossover* (* (/ *percent-crossover* 100) *population-size*))
(defconstant *nr-mutate* (* (/ *percent-mutate* 100) *population-size*))

(defconstant *image-size* 16)
(defconstant *pixel-range* 6)

(defconstant *fitness-exponent* 1.2)

(defconstant *filepath* "C:/Users/Gage/Documents/Programming/Eclipse Workspace/Genetic Image Display/img.txt")

;; Runs the genetic algorithm
(defmethod ga
((verbose t) &aux pop)
  (setf pop
    (make-instance 'image-population
      :images (create-population *pixel-range* *image-size* *population-size*)
    )
  )
  
  (display-generation pop verbose)
  
  (dotimes (i *nr-generations*)
    (setf pop (next-generation pop))
    (display-generation pop verbose)
  )
  
  (format t "Highest fitness: ~A~%" (float (image-fitness (fittest-image pop))))
  (display (fittest-image pop))
  (write-image-to-file (fittest-image pop))
)

;; Generates the next population generation
(defmethod next-generation
((pop image-population) &aux next-pop)
  (setf next-pop
    (make-instance 'image-population
      :images '()
      :generation (+ 1 (population-generation pop))
    )
  )
  (perform-copies pop next-pop)
  (perform-crossovers pop next-pop)
  next-pop
)

;; Performs the copies/mutations for one generation
(defmethod perform-copies
((pop image-population) (next-pop image-population) &aux img)
  (dotimes (i *nr-mutate*)
    (setf img (select pop *sample-size*))
    (maybe-mutate img *mutation-chance* *mutation-degree*)
    (setf
      (population-images next-pop)
      (cons img (population-images next-pop))
    )
  )
)

;; Performs the crossovers for one generation
(defmethod perform-crossovers
((pop image-population) (next-pop image-population))
  (dotimes (i *nr-crossover*)
    (setf
      (population-images next-pop)
      (cons
        (crossover (select pop *sample-size*) (select pop *sample-size*))
        (population-images next-pop)
      )
    )
  )
)

;; Displays the average fitness and generation of 'pop'
(defmethod display-generation
((pop image-population) (display-image t))
  (format t
    "Average fitness (gen ~A): ~A~%"
    (population-generation pop)
    (float (average-fitness pop))
  )
  (if display-image
    (let ()
      (format t "Fittest image:~%")
      (display (fittest-image pop))
    )
  )
)

;; Writes 'img' to '*filepath*'
(defmethod write-image-to-file
((img image))
  (with-open-file
    (str *filepath*
      :direction :output
      :if-exists :supersede
    )
    (format str "~A~A" *pixel-range* (to-string img))
  )
)
