(defclass flip-board ()
  (
    (board :accessor flip-board :initarg :board)
  )
)

;; Generates a 2D list of nil elements
(defmethod null-board
((size integer))
  (null-board-recurse '() size size)
)

;; Recursive supplementary method for null-board
(defmethod null-board-recurse
((strings list) (size integer) (n integer))
  (if (= n 0)
    strings
    (cons (null-string size) (null-board-recurse strings size (- n 1)))
  )
)

;; Generates a string of nil of length 'len'
(defmethod null-string
((len integer))
  (null-string-recurse '() len)
)

;; Recursive supplementary method for null-string
(defmethod null-string-recurse
((string list) (n integer))
  (if (= n 0)
    string
    (cons nil (null-string-recurse string (- n 1)))
  )
)

;; Makes a flip board of size 'size'
(defmethod make-flip-board
((size integer))
  (make-instance 'flip-board
    :board (null-board size)
  )
)

;; Flips the value at 'x' and 'y' for 'fb'
(defmethod flip-to-true
((fb flip-board) (x integer) (y integer))
  (setf
    (nth
      x
      (nth y (flip-board fb))
    )
    t
  )
)

;; Determines if 'fb' is flipped at 'x' 'y'
(defmethod flipped
((fb flip-board) (x integer) (y integer))
  (nth
    x
    (nth y (flip-board fb))
  )
)

;; Displays 'fb'
; (defmethod display
; ((fb flip-board))
  ; (display-recurse (flip-board fb))
  ; (format t "~%")
; )

;; Determines if 'x' and 'y' are both within or on
;; the range 0 - 'bound'
(defmethod in-bounds
((x integer) (y integer) (bound integer))
  (and
    (>= x 0)
    (>= y 0)
    (< x bound)
    (< y bound)
  )
)

;; Determines the size of the cluster at 'x' 'y' in 'img'
(defmethod cluster-size
((img image) (fb flip-board) (x integer) (y integer))
  (cluster-size-recurse img fb x y (pixel img x y))
)

;; Recursive supplementary method for cluster-size
(defmethod cluster-size-recurse
((img image) (fb flip-board) (x integer) (y integer) (start-pixel integer))
  (if
    (or
      (not (in-bounds x y (image-size img)))
      (flipped fb x y)
      (not (= (pixel img x y) start-pixel))
    )
    0
    (let ()
      (flip-to-true fb x y)
      (+
        1
        (cluster-size-recurse img fb (- x 1) y start-pixel)
        (cluster-size-recurse img fb (+ x 1) y start-pixel)
        (cluster-size-recurse img fb x (- y 1) start-pixel)
        (cluster-size-recurse img fb x (+ y 1) start-pixel)
      )
    )
  )
)
