(defun demo1 ()
  (prl "Running Demo 1 (Number strings and pixel arrays)")
  (prl "")
  
  (prl "Here's some number strings of variable range and length")
  (pr "Range 4, length 8: ") (prl (number-string 4 8))
  (pr "Range 8, length 10: ") (prl (number-string 8 10))
  (pr "Range 10, length 32: ") (prl (number-string 10 32))
  (prl "")
  
  (prl "Here's some pixel arrays")
  (prl "Range 4, size 8: ") (display-pixels (make-pixels 4 8))
  (prl "Range 8, size 10: ") (display-pixels (make-pixels 8 10))
  (prl "Range 10, size 20: ") (display-pixels (make-pixels 10 20))
)

(defun demo2 (&aux img)
  (prl "Running Demo 2 (Mutations)")
  (prl "")
  
  (prl "Here's an image")
  (setf img (make-image 10 5))
  (display img)
  (prl "Here's the same image with pixel (1, 2) mutated")
  (mutate-pixel img 1 2)
  (display img)
  (prl "Here's the same image with pixel (0, 0) mutated")
  (mutate-pixel img 0 0)
  (display img)
  (prl "Here's the same image with pixel (0, 1) mutated")
  (mutate-pixel img 0 1)
  (display img)
  (prl "Here's the same image mutated randomly 5 times")
  (mutate img 5)
  (display img)
  (prl "Here's the same image mutated randomly 3 times")
  (mutate img 3)
  (display img)
  (prl "Here's the same image mutated randomly 1 times")
  (mutate img 1)
  (display img)
)

(defun demo3 (&aux img1 img2)
  (prl "Running Demo 3 (Crossover)")
  (prl "")
  
  (prl "Here are 2 images")
  (setf img1 (make-image 4 5))
  (setf img2 (make-image 4 5))
  (prl "Image 1")
  (display img1)
  (prl "Image 2")
  (display img2)
  (prl "Here is the result of crossing the 2 images over")
  (pr "Image 1's largest cluster is at (")
  (pr (image-best-cluster-x img1)) (pr ", ") (pr (image-best-cluster-y img1)) (prl ")")
  (display (crossover img1 img2))
)

(defun demo5 (&aux img)
  (prl "Running Demo 5 (Fitness Metric)")
  (prl "")
  
  (prl "Here's an image")
  (setf img (make-image 4 10))
  (display img)
  (pr "Its fitness is ") (prl (float (image-fitness img)))
  (prl "")
  
  (prl "Here's another image")
  (setf img (make-image 6 10))
  (display img)
  (pr "Its fitness is ") (prl (float (image-fitness img)))
  (prl "")
  
  (prl "Here's another image")
  (setf img (make-image 8 10))
  (display img)
  (pr "Its fitness is ") (prl (float (image-fitness img)))
)

(defun demo7 (&aux pop sample)
  (prl "Running Demo 7 (Population)")
  (prl "")
  
  (prl "Here's a population of 5 images")
  (setf pop
    (make-instance 'image-population
      :images (create-population 10 5 5)
    )
  )
  (display pop)
  (prl "")
  
  (prl "Here's the fittest image")
  (display (fittest-image pop))
  (prl "")
  
  (prl "Here's a sample of size 2 of the population")
  (setf sample (sample pop 2))
  (display (first sample))
  (pr "Fitness = ") (prl (float (image-fitness (first sample))))
  (display (second sample))
  (pr "Fitness = ") (prl (float (image-fitness (second sample))))
  (prl "The fittest image from the sample is")
  (display (fittest-image sample))
  (prl "")
  
  (prl "Here's another sample of size 2 of the population")
  (setf sample (sample pop 2))
  (display (first sample))
  (pr "Fitness = ") (prl (float (image-fitness (first sample))))
  (display (second sample))
  (pr "Fitness = ") (prl (float (image-fitness (second sample))))
  (prl "The fittest image from the sample is")
  (display (fittest-image sample))
  (prl "")
  
  (prl "Here are some selections from the population")
  (display (select pop 2))
  (prl "")
  (display (select pop 2))
  (prl "")
  (display (select pop 2))
)

(defun demo8 (&aux img)
  (prl "Here's a display of the \"maybe mutate\" method")
  (setf img (make-image 5 5))
  (display img)
  (prl "")
  (dotimes (i 10)
    (if (maybe-mutate img *mutation-chance* *mutation-degree*)
      (let ()
        (display img)
        (prl "^^ Mutated ^^")
        (prl "")
      )
      (let ()
        (display img)
        (prl "")
      )
    )
  )
)

;; Displays a pixel array
(defun display-pixels (pixels)
  (dotimes (i (length pixels))
    (prl (nth i pixels))
  )
)

;; Prints 'str' to console
(defun pr (str)
  (format t "~A" str)
)

;; Prints 'str' to console and prints a new line
(defun prl (str)
  (format t "~A~%" str)
)
