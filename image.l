(defclass image ()
  (
    (pixels :accessor image-pixels :initarg :pixels)
    (range :accessor image-range :initarg :range)
    (size :accessor image-size :initarg :size)
    (fitness :accessor image-fitness :initform 0)
    (best-cluster-x :accessor image-best-cluster-x :initform 0)
    (best-cluster-y :accessor image-best-cluster-y :initform 0)
  )
)

;; Generates a string of length 'len' numbers 0 through 'range'
(defmethod number-string
((range integer) (len integer))
  (number-string-recurse '() range len)
)

;; Recursive supplement method for number-string
(defmethod number-string-recurse
((string list) (range integer) (n integer))
  (if (= n 0)
    string
    (number-string-recurse
      (cons (random range) string)
      range
      (- n 1)
    )
  )
)

;; Generates the pixels for an image with range 'range'
;; and image size 'size'
(defmethod make-pixels
((range integer) (size integer))
  (make-pixels-recurse '() range size size)
)

;; Recursive supplement method for make-pixels
(defmethod make-pixels-recurse
((lists list) (range integer) (size integer) (n integer))
  (if (= n 0)
    lists
    (make-pixels-recurse
      (cons (number-string range size) lists)
      range
      size
      (- n 1)
    )
  )
)

;; Makes an image with range 'range' and size 'size'
(defmethod make-image
((range integer) (size integer) &aux img)
  (setf img
    (make-instance 'image
      :pixels (make-pixels range size)
      :range range
      :size size
    )
  )
  (setf (image-fitness img) (fitness img))
  img
)

;; Displays 'img'
(defmethod display
((img image))
  (format t "~A~%" (to-string img))
)

;; Generates a string representation of 'img'
(defmethod to-string
((img image))
  (to-string-recurse (image-pixels img))
)

;; Recursive supplementary method for to-string
(defmethod to-string-recurse
((pixel-lists list))
  (if (not (null pixel-lists))
    (format nil "~A~%~A" (car pixel-lists) (to-string-recurse (cdr pixel-lists)))
    ""
  )
)

;; Mutates 'img' 'times' times
(defmethod mutate
((img image) (times integer))
  (if (> times 0)
    (let ()
      (mutate-pixel
        img
        (random (image-size img))
        (random (image-size img))
      )
      (mutate img (- times 1))
    )
  )
)

;; Mutates the pixel in 'img' at 'x' and 'y'
(defmethod mutate-pixel
((img image) (x integer) (y integer))
  (set-pixel img x y (random (image-range img)))
)

;; Sets the pixel at 'x' 'y' in 'img' to 'new-pixel'
(defmethod set-pixel
((img image) (x integer) (y integer) (new-pixel integer))
  (setf
    (nth
      x
      (nth y (image-pixels img))
    )
    new-pixel
  )
)

;; Possibly mutates 'img' with probability 'p' 'times' times
(defmethod maybe-mutate
((img image) (p float) (times integer))
  (if (< (random 100) (* p 100))
    (let ()
      (mutate img times)
      t
    )
    nil
  )
)

;; Fetches the pixel at 'x' 'y' from 'img'
(defmethod pixel
((img image) (x integer) (y integer))
  (nth
    x
    (nth y (image-pixels img))
  )
)

;; Determines the fitness of 'img'
;; The fitness is determined by the number of and size of clusters
;; of like types
(defmethod fitness
((img image) &aux fb fitness cluster-size best-cluster best-cluster-x best-cluster-y)
  (setf fb (make-flip-board (image-size img)))
  (setf fitness 0)
  (setf best-cluster 0)
  (setf best-cluster-x 0)
  (setf best-cluster-y 0)
  (dotimes (x (image-size img))
    (dotimes (y (image-size img))
      (setf cluster-size (cluster-size img fb x y))
      
      (if (> cluster-size best-cluster)
        (let ()
          (setf best-cluster cluster-size)
          (setf best-cluster-x x)
          (setf best-cluster-y y)
        )
      )
      
      ; (cond
        ; ((= cluster-size 2) (setf fitness (+ fitness 1)))
        ; ((= cluster-size 3) (setf fitness (+ fitness 3)))
        ; ((= cluster-size 4) (setf fitness (+ fitness 7)))
        ; ((= cluster-size 5) (setf fitness (+ fitness 13)))
        ; ((> cluster-size 5) (setf fitness (- fitness 7)))
      ; )
      
      (setf
        fitness
        (+
          fitness
          (expt *fitness-exponent* cluster-size)
        )
      )
    )
  )
  
  (setf (image-best-cluster-x img) best-cluster-x)
  (setf (image-best-cluster-y img) best-cluster-y)
  
  fitness
)

;; Creates a child image based on 'img1' and 'img2'
(defmethod crossover
((img1 image) (img2 image) &aux fb pixels mark img)
  (setf fb (find-biggest-cluster img1))
  (setf img
    (make-instance 'image
      :pixels (null-board (image-size img1))
      :range (image-range img1)
      :size (image-size img1)
    )
  )
  
  (dotimes (x (image-size img))
    (dotimes (y (image-size img))
      (if (flipped fb x y)
        (set-pixel img x y (pixel img1 x y))
        (set-pixel img x y (pixel img2 x y))
      )
    )
  )
  
  (setf (image-fitness img) (fitness img))
  img
)

;; Creates a flip-board where the only true flips represent
;; the largest cluster of 'img'
(defmethod find-biggest-cluster
((img image) &aux fb)
  (setf fb (make-flip-board (image-size img)))
  (cluster-size
    img
    fb
    (image-best-cluster-x img)
    (image-best-cluster-y img)
  )
  fb
)
