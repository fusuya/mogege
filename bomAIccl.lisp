
(load "./AI/json-pasa.lisp" :external-format :utf-8)

;;;;AI?
(defparameter *kabe-list* nil)
(defparameter *block-list* nil)
(defparameter *fire-list* nil)
(defparameter *bomb-pos-list* nil)
(defparameter *move-list* nil)
;;隣にブロックあったらボム置く
(defun bomb-oku? (myposx myposy)
  (let* ((mypos-r (list (+ myposx 1) myposy))
         (mypos-l (list (- myposx 1) myposy))
         (mypos-u (list myposx (- myposy 1)))
         (mypos-d (list myposx (+ myposy 1))))
    (cond
      ((equal (move? myposx myposy) "STAY") "false")
      ((find mypos-r *block-list* :test #'equal) "true")
      ((find mypos-l *block-list* :test #'equal) "true")
      ((find mypos-u *block-list* :test #'equal) "true")
      ((find mypos-d *block-list* :test #'equal) "true")
      (t "false"))))

(defun up? (myposx myposy)
  (let* ((next-posy (decf myposy))
         (next-pos (list myposx next-posy)))
    (cond
      ((< next-posy 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *fire-list*     :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      (t (push "UP" *move-list*)))))

(defun down? (myposx myposy)
  (let* ((next-posy (incf myposy))
         (next-pos (list myposx next-posy)))
    (cond
      ((> next-posy 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *fire-list*     :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      (t (push "DOWN" *move-list*)))))

(defun right? (myposx myposy)
  (let* ((next-posx (incf myposx))
         (next-pos (list next-posx myposy)))
    (cond
      ((> next-posx 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *fire-list*     :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      (t (push "RIGHT" *move-list*)))))

(defun left? (myposx myposy)
  (let* ((next-posx (decf myposx))
         (next-pos (list next-posx myposy)))
    (cond
      ((< next-posx 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *fire-list*     :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      (t (push "LEFT" *move-list*)))))

(defun move? (myposx myposy)
  (up? myposx myposy)
  (down? myposx myposy)
  (right? myposx myposy)
  (left? myposx myposy)
  (if (not *move-list*)
      "STAY"
      (if (>= 1 (length *move-list*))
          (nth 0 *move-list*)
          (nth (random (- (length *move-list*) 1)) *move-list*))))

;;ボム位置list
(defun make-bomb-pos-list (bomb-info)
  (loop for i from 0 to (- (length bomb-info) 1)
        do
        (let ((bomb-pos (list (cdr (second (assoc ':pos (nth i bomb-info))))
                              (cdr (third (assoc ':pos (nth i bomb-info)))))))
          (push bomb-pos *bomb-pos-list*))))
  
;;火の予想位置リスト
(defun make-fire-list (bomb-info)
  (loop for i from 0 to (- (length bomb-info) 1)
        do
        ;;ボム情報 x y
        (let* ((bomb-posx (cdr (second (assoc ':pos (nth i bomb-info)))))
               (bomb-posy (cdr (third (assoc ':pos (nth i bomb-info)))))
               ;;ボムパワー
               (bomb-p (cdr (assoc ':power (nth i bomb-info))))
               (fire-posx-min (- bomb-posx bomb-p))
               (fire-posx-max (+ bomb-posx bomb-p))
               (fire-posy-min (- bomb-posy bomb-p))
               (fire-posy-max (+ bomb-posy bomb-p)))
          (loop for z from fire-posx-min to fire-posx-max
                for y from fire-posy-min to fire-posy-max
                do (progn
                     (if (and (<= 1 z) (>= 13 z))
                         (push (list z bomb-posy) *fire-list*))
                     (if (and (<= 1 y) (>= 13 y))
                         (push (list bomb-posx y) *fire-list*)))))))

(defun hoge () 
  (princ "もげげ")
  (fresh-line)
  (let ((id (read-line)))
    (loop
      (let* ((mapdata (with-input-from-string (in (read-line)) (read-json in)))
             (my-info (nth (+ (parse-integer id) 1) (fourth mapdata)))
             (myposx (cdr (second (second my-info))))
             (myposy (cdr (third (second my-info))))
             ;;(mypos (list myposx myposy))
             (bomb-info (cdr (assoc ':bombs mapdata))))
        (setf *kabe-list* (cdr (second mapdata))
              *block-list* (cdr (third mapdata))
              *fire-list* nil
              *bomb-pos-list* nil
              *move-list* nil)
        (make-bomb-pos-list bomb-info)
        (make-fire-list bomb-info)
        (princ (concatenate 'string (move? myposx myposy)
                             "," 
                             (bomb-oku? myposx myposy)))
        (fresh-line)))))

(hoge)


