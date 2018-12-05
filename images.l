(defclass image-population ()
  (
    (images :accessor population-images :initarg :images)
    (generation :accessor population-generation :initarg :generation :initform 0)
  )
)

;; Creates a population of size 'n' with images with range 'range'
;; and size 'size'
(defmethod create-population
((range integer) (size integer) (n integer))
  (if (= n 0)
    '()
    (cons
      (make-image range size)
      (create-population range size (- n 1))
    )
  )
)

;; Displays 'pop'
(defmethod display
((pop image-population))
  (dotimes (i (length (population-images pop)))
    (display (nth i (population-images pop)))
    (format t "~%")
  )
)

;; Selects a random sample of images from 'pop'
(defmethod sample
((pop image-population) (n integer))
  (if (= n 0)
    '()
    (cons
      (nth
        (random (length (population-images pop)))
        (population-images pop)
      )
      (sample pop (- n 1))
    )
  )
)

;; Selects the fittest image from 'sample'
(defmethod fittest-image
((sample list))
  (if (= (length sample) 2)
    (if (> (image-fitness (first sample)) (image-fitness (second sample)))
      (first sample)
      (second sample)
    )
    (let (fittest)
      (setf fittest (fittest-image (cdr sample)))
      (if (> (image-fitness (car sample)) (image-fitness fittest))
        (car sample)
        fittest
      )
    )    
  )
)

;; Selects the fittest image in the whole 'pop'
(defmethod fittest-image
((pop image-population))
  (fittest-image (population-images pop))
)

;; Samples 'sample-size' images from 'pop' and selects
;; the fittest image from the sample
(defmethod select
((pop image-population) (sample-size integer))
  (fittest-image (sample pop sample-size))
)

;; Calculates the average fitness of 'pop'
(defmethod average-fitness
((pop image-population))
  (/
    (reduce #'+ (mapcar #'image-fitness (population-images pop)))
    (length (population-images pop))
  )
)
