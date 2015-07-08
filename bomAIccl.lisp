(load "./AI/json-pasa.lisp")
(setf *random-state* (make-random-state t))
;;;;AI?
(defparameter *kabe-list* nil)
(defparameter *block-list* nil)
(defparameter *fire-list* nil)
(defparameter *bomb-pos-list* nil)
(defparameter *simbombpos-list* nil)
(defparameter *move-list* nil)
(defparameter *safe-list* nil)
(defparameter *safe-list-kamo* nil)
(defparameter *simfire-list* nil)
(defparameter *okanai* nil)
(defparameter *hoge* nil)
(defparameter *sin-move-list* nil)
(defparameter *enemy0posx* nil)
(defparameter *enemy0posy* nil)
(defparameter *enemy0pos* nil)
(defparameter *enemy0bombp* nil)
(defparameter *enemy1posx* nil)
(defparameter *enemy1posy* nil)
(defparameter *enemy1pos* nil)
(defparameter *enemy1bombp* nil)
(defparameter *enemy2posx* nil)
(defparameter *enemy2posy* nil)
(defparameter *enemy2pos* nil)
(defparameter *enemy2bombp* nil)
(defparameter *enemy3posx* nil)
(defparameter *enemy3posy* nil)
(defparameter *enemy3pos* nil)
(defparameter *enemy3bombp* nil)
(defparameter *yusen-move-list* nil)
(defparameter *kabe1* '((1 1) (2 1) (3 1) (4 1) (5 1) (6 1) (7 1) (8 1) (9 1) (10 1) (11 1) (12 1) (13 1) (13 2) (13 3) (13 4) (13 5) (13 6) (13 7) (13 8) (13 9) (13 10) (13 11) (13 12) (13 13) (12 13) (11 13) (10 13) (9 13) (8 13) (7 13) (6 13) (5 13) (4 13) (3 13) (2 13) (1 13) (1 12) (1 11) (1 10) (1 9) (1 8) (1 7) (1 6) (1 5) (1 4) (1 3) (1 2)))
(defparameter *kabe2* '((3 2) (5 2) (7 2) (9 2) (11 2) (12 3) (12 5) (12 7) (12 9) (12 11) (11 12) (9 12) (7 12) (5 12) (3 12) (2 11) (2 9) (2 7) (2 5) (2 3)))
(defparameter *kabe3* '((3 3) (4 3) (5 3) (6 3) (7 3) (8 3) (9 3) (10 3) (11 3) (11 4) (11 5) (11 6) (11 7) (11 8) (11 9) (11 10) (11 11) (10 11) (9 11) (8 11) (7 11) (6 11) (5 11) (4 11) (3 11) (3 10) (3 9) (3 8) (3 7) (3 6) (3 5) (3 4)))
(defparameter *kabe4* '((5 4) (7 4) (9 4) (10 5) (10 7) (10 9) (9 10) (7 10) (5 10) (4 9) (4 7) (4 5)))

(defun up? (myposx myposy mypos)
  (let* ((next-posy (- myposy 1))
         (next-pos (list myposx next-posy)))
    (cond
      ((< next-posy 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ;;((member next-pos *fire-list*     :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ((and (not (member mypos *fire-list* :test #'equal))
            (member next-pos *fire-list* :test #'equal))
       nil)
      ((and (not (member mypos *simfire-list* :test #'equal))
            (member next-pos *simfire-list* :test #'equal)) nil)
      ((and (member mypos *fire-list* :test #'equal)
            (not (member next-pos *fire-list* :test #'equal))
            (member mypos *simfire-list* :test #'equal)
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "UP" *safe-list*))
      (t (push "UP" *move-list*)))))

(defun down? (myposx myposy mypos)
  (let* ((next-posy (+ myposy 1))
         (next-pos (list myposx next-posy)))
    (cond
      ((> next-posy 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ;;((member next-pos *fire-list*     :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ((and (not (member mypos *fire-list* :test #'equal))
            (member next-pos *fire-list* :test #'equal))
       nil)
      ((and (not (member mypos *simfire-list* :test #'equal))
            (member next-pos *simfire-list* :test #'equal)) nil)
      ((and (member mypos *fire-list* :test #'equal)
            (not (member next-pos *fire-list* :test #'equal))
            (member mypos *simfire-list* :test #'equal)
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "DOWN" *safe-list*))
      (t (push "DOWN" *move-list*)))))

(defun right? (myposx myposy mypos)
  (let* ((next-posx (+ myposx 1))
         (next-pos (list next-posx myposy)))
    (cond
      ((> next-posx 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ;;((member next-pos *fire-list*     :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ((and (not (member mypos *fire-list* :test #'equal))
            (member next-pos *fire-list* :test #'equal))
       nil)
      ((and (not (member mypos *simfire-list* :test #'equal))
            (member next-pos *simfire-list* :test #'equal)) nil)
      ((and (member mypos *fire-list* :test #'equal)
            (not (member next-pos *fire-list* :test #'equal))
            (member mypos *simfire-list* :test #'equal)
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "RIGHT" *safe-list*))
      (t (push "RIGHT" *move-list*)))))

(defun left? (myposx myposy mypos)
  (let* ((next-posx (- myposx 1))
         (next-pos (list next-posx myposy)))
    (cond
      ((< next-posx 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ;;((member next-pos *fire-list*     :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ((and (not (member mypos *fire-list* :test #'equal))
            (member next-pos *fire-list* :test #'equal)) nil)
      ((and (not (member mypos *simfire-list* :test #'equal))
            (member next-pos *simfire-list* :test #'equal)) nil)
      ((and (member mypos *fire-list* :test #'equal)
            (not (member next-pos *fire-list* :test #'equal))
            (member mypos *simfire-list* :test #'equal)
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "LEFT" *safe-list*))
      (t (push "LEFT" *move-list*)))))

(defun move? (myposx myposy mypos)
  (up? myposx myposy mypos)
  (down? myposx myposy mypos)
  (right? myposx myposy mypos)
  (left? myposx myposy mypos)
  (cond 
    (*safe-list*
      (nth (random (length *safe-list*)) *safe-list*))
    (*safe-list-kamo*
      (nth (random (length *safe-list-kamo*)) *safe-list-kamo*))
    (*move-list*
      (nth (random (length *move-list*)) *move-list*))
    (t "STAY")))

;;i=ループ回数
(defun 10move-m (myposx myposy i mypos)
  (setf *move-list* nil
        *safe-list-kamo* nil
        *safe-list* nil)
  (let ((dir (move? myposx myposy mypos)))
    (cond 
      ((>= 0 i) (if (member (list myposx myposy) *fire-list* :test #'equal)
                    nil
                    t))
      ((equal "UP" dir)
       (10move-m myposx (- myposy 1) (decf i) (list myposx myposy)))
      ((equal "LEFT" dir)
       (10move-m (- myposx 1) myposy (decf i) (list myposx myposy)))
      ((equal "DOWN" dir)
       (10move-m myposx (+ myposy 1) (decf i) (list myposx myposy)))
      ((equal "RIGHT" dir)
       (10move-m (+ myposx 1) myposy (decf i) (list myposx myposy)))      
      (t (if (member (list myposx myposy) *fire-list* :test #'equal)
                    nil
                    t)))))
;;爆弾をおけない状態で移動先を探す
(defun sim10m (posx posy pos)
  (setf *sin-move-list* nil
        *yusen-move-list* nil)
  (if *enemy0pos*
      (make-simfire-list *enemy0posx* *enemy0posy* *enemy0bombp* *enemy0pos*))
  (if *enemy1pos*
      (make-simfire-list *enemy1posx* *enemy1posy* *enemy1bombp* *enemy1pos*))
  (if *enemy2pos*
      (make-simfire-list *enemy2posx* *enemy2posy* *enemy2bombp* *enemy2pos*))
  (if *enemy3pos*
      (make-simfire-list *enemy3posx* *enemy3posy* *enemy3bombp* *enemy3pos*))
  (cond ((godown? posx posy pos)
         (if (and (member pos *fire-list* :test #'equal)
                  (not (member (list posx (+ posy 1)) *fire-list* :test #'equal)))
             (push "DOWN" *yusen-move-list*))
         (setf *hoge* nil)
         (loop for i from 0 to 20
               do
               (push (10move-m posx (+ posy 1) 6 pos) *hoge*))
         (if (> (count t *hoge*) 10)
             (push "DOWN" *sin-move-list*))))
  (cond ((goup? posx posy pos)
         (if (and (member pos *fire-list* :test #'equal)
                  (not (member (list posx (- posy 1)) *fire-list* :test #'equal)))
             (push "UP" *yusen-move-list*))
         (setf *hoge* nil)
         (loop for i from 0 to 20
               do
               (push (10move-m posx (- posy 1) 6 pos) *hoge*))
         (if (> (count t *hoge*) 10)
             (push "UP" *sin-move-list*))))
  (cond ((goright? posx posy pos)
         (if (and (member pos *fire-list* :test #'equal)
                  (not (member (list (+ posx 1) posy) *fire-list* :test #'equal)))
             (push "RIGHT" *yusen-move-list*))
         (setf *hoge* nil)
         (loop for i from 0 to 20
               do
               (push (10move-m (+ posx 1) posy 6 pos) *hoge*))
         (if (> (count t *hoge*) 10)
             (push "RIGHT" *sin-move-list*))))
  (cond ((goleft? posx posy pos)
         (if (and (member pos *fire-list* :test #'equal)
                  (not (member (list (- posx 1) posy) *fire-list* :test #'equal)))
             (push "LEFT" *yusen-move-list*))
         (setf *hoge* nil)
         (loop for i from 0 to 20
               do
               (push (10move-m (- posx 1) posy 6 pos) *hoge*))
         (if (> (count t *hoge*) 10)
             (push "LEFT" *sin-move-list*))))
  (cond 
    (*yusen-move-list*
      (nth (random (length *yusen-move-list*)) *yusen-move-list*))
    (*sin-move-list*
      (nth (random (length *sin-move-list*)) *sin-move-list*))
    (t "STAY")))



(defun goup? (myposx myposy mypos)
  (let* ((next-posy (- myposy 1))
         (next-pos (list myposx next-posy)))
    (cond
      ((< next-posy 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((and (not (member mypos *fire-list* :test #'equal))
            (member next-pos *fire-list* :test #'equal)) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      (t t))))

(defun godown? (myposx myposy mypos)
  (let* ((next-posy (+ myposy 1))
         (next-pos (list myposx next-posy)))
    (cond
      ((> next-posy 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((and (not (member mypos *fire-list* :test #'equal))
            (member next-pos *fire-list* :test #'equal)) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      (t t))))

(defun goright? (myposx myposy mypos)
  (let* ((next-posx (+ myposx 1))
         (next-pos (list next-posx myposy)))
    (cond
      ((> next-posx 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((and (not (member mypos *fire-list* :test #'equal))
            (member next-pos *fire-list* :test #'equal)) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      (t t))))

(defun goleft? (myposx myposy mypos)
  (let* ((next-posx (- myposx 1))
         (next-pos (list next-posx myposy)))
    (cond
      ((< next-posx 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((and (not (member mypos *fire-list* :test #'equal))
            (member next-pos *fire-list* :test #'equal)) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      (t t))))


;;ボム位置list
(defun make-bomb-pos-list (bomb-info)
  (loop for i from 0 to (- (length bomb-info) 1)
        do
        (let ((bomb-pos (list (cdr (second (assoc ':pos (nth i bomb-info))))
                              (cdr (third (assoc ':pos (nth i bomb-info)))))))
          (push bomb-pos *bomb-pos-list*))))


;;火の予想位置リスト
(defun fire-list-dayo (bomb-posx bomb-posy bomb-p)
  (push (list bomb-posx bomb-posy) *fire-list*)
  (loop for x from 1 to bomb-p
        do
        (cond
          ((or (> (+ bomb-posx x) 13)
               ;;(equal mypos (list (+ bomb-posx x) bomb-posy))
               (member (list (+ bomb-posx x) bomb-posy) *kabe-list* :test #'equal)
               (member (list (+ bomb-posx x) bomb-posy) *block-list* :test #'equal))
           (return))
          (t (push (list (+ bomb-posx x) bomb-posy) *fire-list*))))
  (loop for x from 1 to bomb-p
        do
        (cond
          ((or (< (- bomb-posx x) 1)
               ;;(equal (list (- bomb-posx x) bomb-posy) mypos)
               (member (list (- bomb-posx x) bomb-posy) *kabe-list* :test #'equal)
               (member (list (- bomb-posx x) bomb-posy) *block-list* :test #'equal))
           (return))
          (t (push (list (- bomb-posx x) bomb-posy) *fire-list*))))
  (loop for y from 1 to bomb-p
        do
        (cond
          ((or (< (- bomb-posy y) 1)
               ;;(equal (list bomb-posx (- bomb-posy y)) mypos)
               (member (list bomb-posx (- bomb-posy y)) *kabe-list* :test #'equal)
               (member (list bomb-posx (- bomb-posy y)) *block-list* :test #'equal))
           (return))
          (t (push (list bomb-posx (- bomb-posy y)) *fire-list*))))
  (loop for y from 1 to bomb-p
        do
        (cond
          ((or (> (+ bomb-posy y) 13)
               ;;(equal (list bomb-posx (+ bomb-posy y)) mypos)
               (member (list bomb-posx (+ bomb-posy y)) *kabe-list* :test #'equal)
               (member (list bomb-posx (+ bomb-posy y)) *block-list* :test #'equal))
           (return))
          (t (push (list bomb-posx (+ bomb-posy y)) *fire-list*)))))
;;おいてある爆弾の火のリストを作る
(defun make-fire-list2 (bomb-info)
  (loop for i from 0 to (- (length bomb-info) 1)
        do
        (let* ((bomb-posx (cdr (second (assoc ':pos (nth i bomb-info)))))
               (bomb-posy (cdr (third (assoc ':pos (nth i bomb-info)))))
               ;;ボムパワー
               (bomb-p (cdr (assoc ':power (nth i bomb-info)))))
          (fire-list-dayo bomb-posx bomb-posy bomb-p))))

;;今いる場所に爆弾をおいた場合の火のリストを作る
(defun make-simfire-list (myposx myposy mybomb-p mypos)
  (push mypos *simfire-list*)
  (loop for x from 1 to mybomb-p
        do
        (cond
          ((or (> (+ myposx x) 13)
               ;;(equal mypos (list (+ myposx x) myposy))
               (member (list (+ myposx x) myposy) *kabe-list* :test #'equal)
               (member (list (+ myposx x) myposy) *block-list* :test #'equal))
           (return))
          (t (push (list (+ myposx x) myposy) *simfire-list*))))
  (loop for x from 1 to mybomb-p
        do
        (cond
          ((or (< (- myposx x) 1)
               ;;(equal (list (- myposx x) myposy) mypos)
               (member (list (- myposx x) myposy) *kabe-list* :test #'equal)
               (member (list (- myposx x) myposy) *block-list* :test #'equal))
           (return))
          (t (push (list (- myposx x) myposy) *simfire-list*))))
  (loop for y from 1 to mybomb-p
        do
        (cond
          ((or (< (- myposy y) 1)
               ;;(equal (list myposx (- myposy y)) mypos)
               (member (list myposx (- myposy y)) *kabe-list* :test #'equal)
               (member (list myposx (- myposy y)) *block-list* :test #'equal))
           (return))
          (t (push (list myposx (- myposy y)) *simfire-list*))))
  (loop for y from 1 to mybomb-p
        do
        (cond
          ((or (> (+ myposy y) 13)
               ;;(equal (list myposx (+ myposy y)) mypos)
               (member (list myposx (+ myposy y)) *kabe-list* :test #'equal)
               (member (list myposx (+ myposy y)) *block-list* :test #'equal))
           (return))
          (t (push (list myposx (+ myposy y)) *simfire-list*)))))
  
(defun simup? (myposx myposy simbombpos mypos)
  (let* ((next-posy (- myposy 1))
         (next-pos (list myposx next-posy)))
    (cond
      ((< next-posy 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ;;((member next-pos *fire-list*     :test #'equal) nil)
      ((and (not (member mypos *fire-list* :test #'equal))
            (member next-pos *fire-list* :test #'equal))
       nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ;;((member next-pos *simfire-list*    :test #'equal) nil)
      ((and (not (member mypos *simfire-list* :test #'equal))
            (member next-pos *simfire-list* :test #'equal))
        nil)
      ((and (or (member mypos *simfire-list* :test #'equal)
                (member mypos *fire-list* :test #'equal))
            (equal next-pos mypos)) nil)
      ((equal next-pos simbombpos) nil)
      ((and (member mypos *fire-list* :test #'equal)
            (not (member next-pos *fire-list* :test #'equal))
            (member mypos *simfire-list* :test #'equal)
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "UP" *safe-list*))
      (t (push "UP" *move-list*)))))

(defun simdown? (myposx myposy simbombpos mypos)
  (let* ((next-posy (+ myposy 1))
         (next-pos (list myposx next-posy)))
    (cond
      ((> next-posy 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ;;((member next-pos *fire-list*     :test #'equal) nil)
      ((and (not (member mypos *fire-list* :test #'equal))
            (member next-pos *fire-list* :test #'equal))
       nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ;;((member next-pos *simfire-list*    :test #'equal) nil)
      ((and (not (member mypos *simfire-list* :test #'equal))
            (member next-pos *simfire-list* :test #'equal)) nil)
      ((and (or (member mypos *simfire-list* :test #'equal)
                (member mypos *fire-list* :test #'equal))
            (equal next-pos mypos)) nil)
      ((equal next-pos simbombpos) nil)
      ((and (member mypos *fire-list* :test #'equal)
            (not (member next-pos *fire-list* :test #'equal))
            (member mypos *simfire-list* :test #'equal)
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "DOWN" *safe-list*))
      (t (push "DOWN" *move-list*)))))

(defun simright? (myposx myposy simbombpos mypos)
  (let* ((next-posx (+ myposx 1))
         (next-pos (list next-posx myposy)))
    (cond
      ((> next-posx 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ;;((member next-pos *fire-list*     :test #'equal) nil)
      ((and (not (member mypos *fire-list* :test #'equal))
            (member next-pos *fire-list* :test #'equal))
       nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ;;((member next-pos *simfire-list*    :test #'equal) nil)
      ((and (not (member mypos *simfire-list* :test #'equal))
            (member next-pos *simfire-list* :test #'equal))
       nil)
      ((and (or (member mypos *simfire-list* :test #'equal)
               (member mypos *fire-list* :test #'equal))
            (equal next-pos mypos)) nil)
      ((equal next-pos simbombpos) nil)
      ((and (member mypos *fire-list* :test #'equal)
            (not (member next-pos *fire-list* :test #'equal))
            (member mypos *simfire-list* :test #'equal)
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "RIGHT" *safe-list*))
      (t (push "RIGHT" *move-list*)))))

(defun simleft? (myposx myposy simbombpos mypos)
  (let* ((next-posx (- myposx 1))
         (next-pos (list next-posx myposy)))
    (cond
      ((< next-posx 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ;;((member next-pos *fire-list*     :test #'equal) nil)
      ((and (not (member mypos *fire-list* :test #'equal))
            (member next-pos *fire-list* :test #'equal))
       nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ;;((member next-pos *simfire-list*    :test #'equal) nil)
      ((and (not (member mypos *simfire-list* :test #'equal))
            (member next-pos *simfire-list* :test #'equal)) nil)
      ((and (or (member mypos *simfire-list* :test #'equal)
                (member mypos *fire-list* :test #'equal))
            (equal next-pos mypos)) nil)
      ((equal next-pos simbombpos) nil)
      ((and (member mypos *fire-list* :test #'equal)
            (not (member next-pos *fire-list* :test #'equal))
            (member mypos *simfire-list* :test #'equal)
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "LEFT" *safe-list*))
      (t (push "LEFT" *move-list*)))))

;;一つ先の場所からさらに上下左右どこへ行けるかさがす
(defun simmove? (myposx myposy simbombpos mypos)
  (simup? myposx myposy simbombpos mypos)
  (simdown? myposx myposy simbombpos mypos)
  (simright? myposx myposy simbombpos mypos)
  (simleft? myposx myposy simbombpos mypos)
  (cond (*safe-list*
         (nth (random (length *safe-list*)) *safe-list*))
        (*safe-list-kamo*
         (nth (random (length *safe-list-kamo*)) *safe-list-kamo*))
        (*move-list*
         (nth (random (length *move-list*)) *move-list*))
        (t nil)))

;;i=ループ回数 i回移動して安全な場所だったらtを返す
(defun 10move-s (myposx myposy i simbombpos mypos)
  (setf *move-list* nil
        *safe-list* nil
        *safe-list-kamo* nil)
  (let ((dir (simmove? myposx myposy simbombpos mypos)))
    (cond 
      ((>= 0 i) (if (or (member (list myposx myposy) *simfire-list* :test #'equal)
                        (member (list myposx myposy) *fire-list* :test #'equal))
                    nil
                    t))
      ((equal "UP" dir)
       (10move-s myposx (- myposy 1) (decf i) simbombpos (list myposx myposy)))
      ((equal "DOWN" dir)
       (10move-s myposx (+ myposy 1) (decf i) simbombpos (list myposx myposy)))
      ((equal "RIGHT" dir)
       (10move-s (+ myposx 1) myposy (decf i) simbombpos (list myposx myposy)))
      ((equal "LEFT" dir)
       (10move-s (- myposx 1) myposy (decf i) simbombpos (list myposx myposy)))
      (t (if (or (member (list myposx myposy) *simfire-list* :test #'equal)
                 (member (list myposx myposy) *fire-list* :test #'equal))
             nil
             t)))))

;;移動先を探す
(defun sim10 (posx posy pos bomb-p)
  (let ((simbombpos pos))
    (setf *sin-move-list* nil
          *yusen-move-list* nil)
    (make-simfire-list posx posy bomb-p pos)
    (if *enemy0pos*
        (make-simfire-list *enemy0posx* *enemy0posy* *enemy0bombp* *enemy0pos*))
    (if *enemy1pos*
        (make-simfire-list *enemy1posx* *enemy1posy* *enemy1bombp* *enemy1pos*))
    (if *enemy2pos*
        (make-simfire-list *enemy2posx* *enemy2posy* *enemy2bombp* *enemy2pos*))
    (if *enemy3pos*
        (make-simfire-list *enemy3posx* *enemy3posy* *enemy3bombp* *enemy3pos*))
    (cond ((godown? posx posy pos)
           (if (and (member pos *fire-list* :test #'equal)
                    (not (member (list posx (+ posy 1)) *fire-list* :test #'equal)))
               (push "DOWN" *yusen-move-list*))
           (setf *hoge* nil)
           (loop for i from 0 to 20
                 do
                 (push (10move-s posx (+ posy 1) 5 simbombpos pos) *hoge*))
           (if (> (count t *hoge*) 16)
               (push "DOWN" *sin-move-list*))))
    (cond ((goup? posx posy pos)
           (if (and (member pos *fire-list* :test #'equal)
                    (not (member (list posx (- posy 1)) *fire-list* :test #'equal)))
               (push "UP" *yusen-move-list*))
           (setf *hoge* nil)
           (loop for i from 0 to 20
                 do
                 (push (10move-s posx (- posy 1) 5 simbombpos pos) *hoge*))
           (if (> (count t *hoge*) 16)
               (push "UP" *sin-move-list*))))
    (cond ((goright? posx posy pos)
           (if (and (member pos *fire-list* :test #'equal)
                    (not (member (list (+ posx 1) posy) *fire-list* :test #'equal)))
               (push "RIGHT" *yusen-move-list*))
           (setf *hoge* nil)
           (loop for i from 0 to 20
                 do
                 (push (10move-s (+ posx 1) posy 5 simbombpos pos) *hoge*))
           (if (> (count t *hoge*) 16)
               (push "RIGHT" *sin-move-list*))))
    (cond ((goleft? posx posy pos)
           (if (and (member pos *fire-list* :test #'equal)
                    (not (member (list (- posx 1) posy) *fire-list* :test #'equal)))
               (push "LEFT" *yusen-move-list*))
           (setf *hoge* nil)
           (loop for i from 0 to 20
                 do
                 (push (10move-s (- posx 1) posy 5 simbombpos pos) *hoge*))
           (if (> (count t *hoge*) 16)
               (push "LEFT" *sin-move-list*))))
    (cond 
      (*yusen-move-list*
       (setf *okanai* t)
       (nth (random (length *yusen-move-list*)) *yusen-move-list*))
      ((null *sin-move-list*)
       (setf *okanai* t)
       (sim10m posx posy pos))
      (t (nth (random (length *sin-move-list*)) *sin-move-list*)))))

;;敵の情報取得
(defun set-enemy-info (id-list mapdata)
  (cond
    ((null id-list) nil)
    ((= 0 (car id-list))
      (setf *enemy0posx* (cdr (second (assoc ':pos (nth (car id-list) (cdr (assoc ':players mapdata))))))
            *enemy0posy* (cdr (third (assoc ':pos (nth (car id-list) (cdr (assoc ':players mapdata)))))))
      (setf *enemy0pos* (list *enemy0posx* *enemy0posy*))
      (setf *enemy0bombp* (cdr (assoc ':power (nth (car id-list) (cdr (assoc ':players mapdata))))))
      (set-enemy-info (cdr id-list) mapdata))
    ((= 1 (car id-list))
      (setf *enemy1posx* (cdr (second (assoc ':pos (nth (car id-list) (cdr (assoc ':players mapdata))))))
            *enemy1posy* (cdr (third (assoc ':pos (nth (car id-list) (cdr (assoc ':players mapdata)))))))
      (setf *enemy1pos* (list *enemy1posx* *enemy1posy*))
      (setf *enemy1bombp* (cdr (assoc ':power (nth (car id-list) (cdr (assoc ':players mapdata))))))
      (set-enemy-info (cdr id-list) mapdata))
    ((= 2 (car id-list))
      (setf *enemy2posx* (cdr (second (assoc ':pos (nth (car id-list) (cdr (assoc ':players mapdata))))))
            *enemy2posy* (cdr (third (assoc ':pos (nth (car id-list) (cdr (assoc ':players mapdata)))))))
      (setf *enemy2pos* (list *enemy2posx* *enemy2posy*))
      (setf *enemy2bombp* (cdr (assoc ':power (nth (car id-list) (cdr (assoc ':players mapdata))))))
      (set-enemy-info (cdr id-list) mapdata))
    ((= 3 (car id-list))
      (setf *enemy3posx* (cdr (second (assoc ':pos (nth (car id-list) (cdr (assoc ':players mapdata))))))
            *enemy3posy* (cdr (third (assoc ':pos (nth (car id-list) (cdr (assoc ':players mapdata)))))))
      (setf *enemy3pos* (list *enemy3posx* *enemy3posy*))
      (setf *enemy3bombp* (cdr (assoc ':power (nth (car id-list) (cdr (assoc ':players mapdata))))))
      (set-enemy-info (cdr id-list) mapdata))))


(defun hoge () 
  (princ "もげげ")
  (fresh-line)
  (let ((id (read-line)))
    (loop
      (let* ((mapdata (with-input-from-string (in (read-line)) (read-json in)))
             (id-list '(0 1 2 3))
             (my-info (nth (+ (parse-integer id) 1) (fourth mapdata)))
             (myposx (cdr (second (second my-info))))
             (myposy (cdr (third (second my-info))))
             (mypos (list myposx myposy))
             (mybomb-p (+ 1 (cdr (third my-info))))
             (mybomblimit (cdr (assoc ':setbomblimit my-info)))
             (turn-n (cdr (assoc ':turn mapdata)))
             (bomb-info (cdr (assoc ':bombs mapdata))))
        (setf *kabe-list* (cdr (second mapdata))
              *block-list* (cdr (third mapdata))
              *fire-list* nil
              *bomb-pos-list* nil
              *simfire-list* nil
              *okanai* nil
              *sin-move-list* nil
              *simbombpos-list* nil)
        (cond
          ((>= turn-n 503) nil)
          ((>= turn-n 479) (setf *kabe-list* (append *kabe4* *kabe-list*)))
          ((>= turn-n 447) (setf *kabe-list* (append *kabe3* *kabe-list*)))
          ((>= turn-n 407) (setf *kabe-list* (append *kabe2* *kabe-list*)))
          ((>= turn-n 360) (setf *kabe-list* (append *kabe1* *kabe-list*))))
        (setf id-list (delete (parse-integer id) id-list))
        (set-enemy-info id-list mapdata)
        (make-bomb-pos-list bomb-info)
        (make-fire-list2 bomb-info)
        (princ (concatenate 'string (if (= 0 mybomblimit)
                                        (sim10m myposx myposy mypos)
                                        (sim10 myposx myposy mypos mybomb-p))
                             "," 
                             (if *okanai*
                                 "false"
                                 "true")))
        (fresh-line)))))

(hoge)


