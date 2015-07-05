(load "./AI/json-pasa.lisp")
(setf *random-state* (make-random-state t))
;;;;AI?
(defparameter *kabe-list* nil)
(defparameter *block-list* nil)
(defparameter *fire-list* nil)
(defparameter *bomb-pos-list* nil)
(defparameter *move-list* nil)
(defparameter *simfire-list* nil)
(defparameter *okanai* nil)
(defparameter *hoge* nil)
(defparameter *sin-move-list* nil)

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
            (member next-pos *fire-list* :test #'equal))
       nil)
      (t (push "LEFT" *move-list*)))))

(defun move? (myposx myposy mypos)
  (up? myposx myposy mypos)
  (down? myposx myposy mypos)
  (right? myposx myposy mypos)
  (left? myposx myposy mypos)
  (if (not *move-list*)
      "STAY"
      ;;(if (>= 1 (length *move-list*))
      ;;   (nth 0 *move-list*)
      (nth (random (length *move-list*)) *move-list*)))

;;i=ループ回数
(defun 10move-m (myposx myposy i mypos)
  (setf *move-list* nil)
  (let ((dir (move? myposx myposy mypos)))
    (cond 
      ((>= 0 i) (if (member (list myposx myposy) *fire-list* :test #'equal)
                    nil
                    t))
      ((equal "UP" dir)
       (10move-m myposx (- myposy 1) (decf i) (list myposx (- myposy 1))))
      ((equal "LEFT" dir)
       (10move-m (- myposx 1) myposy (decf i) (list (- myposx 1) myposy)))
      ((equal "DOWN" dir)
       (10move-m myposx (+ myposy 1) (decf i) (list myposx (+ myposy 1))))
      ((equal "RIGHT" dir)
       (10move-m (+ myposx 1) myposy (decf i) (list (+ myposx 1) myposy)))      
      (t (if (member (list myposx myposy) *fire-list* :test #'equal)
                    nil
                    t)))))

(defun sim10m (posx posy pos)
    (cond ((godown? posx posy)
           (setf *hoge* nil)
           (loop for i from 0 to 5
                 do
                 (push (10move-m posx (+ posy 1) 6 pos) *hoge*))
           (if (> (count t *hoge*) 2)
               (push "DOWN" *sin-move-list*))))
    (cond ((goup? posx posy)
           (setf *hoge* nil)
           (loop for i from 0 to 5
                 do
                 (push (10move-m posx (- posy 1) 6 pos) *hoge*))
           (if (> (count t *hoge*) 2)
               (push "UP" *sin-move-list*))))
    (cond ((goright? posx posy)
           (setf *hoge* nil)
           (loop for i from 0 to 5
                 do
                 (push (10move-m (+ posx 1) posy 6 pos) *hoge*))
           (if (> (count t *hoge*) 2)
               (push "RIGHT" *sin-move-list*))))
    (cond ((goleft? posx posy)
           (setf *hoge* nil)
           (loop for i from 0 to 5
                 do
                 (push (10move-m (- posx 1) posy 6 pos) *hoge*))
           (if (> (count t *hoge*) 2)
               (push "LEFT" *sin-move-list*))))
    (nth (random (length *sin-move-list*)) *sin-move-list*))



(defun goup? (myposx myposy)
  (let* ((next-posy (- myposy 1))
         (next-pos (list myposx next-posy)))
    (cond
      ((< next-posy 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *fire-list*     :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      (t t))))

(defun godown? (myposx myposy)
  (let* ((next-posy (+ myposy 1))
         (next-pos (list myposx next-posy)))
    (cond
      ((> next-posy 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *fire-list*     :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      (t t))))

(defun goright? (myposx myposy)
  (let* ((next-posx (+ myposx 1))
         (next-pos (list next-posx myposy)))
    (cond
      ((> next-posx 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *fire-list*     :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      (t t))))

(defun goleft? (myposx myposy)
  (let* ((next-posx (- myposx 1))
         (next-pos (list next-posx myposy)))
    (cond
      ((< next-posx 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *fire-list*     :test #'equal) nil)
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
(defun fire-list-dayo (bomb-posx bomb-posy bomb-p mypos)
  (loop for x from 1 to bomb-p
        do
        (cond
          ((or (> (+ bomb-posx x) 13)
               (equal mypos (list (+ bomb-posx x) bomb-posy))
               (member (list (+ bomb-posx x) bomb-posy) *kabe-list* :test #'equal)
               (member (list (+ bomb-posx x) bomb-posy) *block-list* :test #'equal))
           (return))
          (t (push (list (+ bomb-posx x) bomb-posy) *fire-list*))))
  (loop for x from 1 to bomb-p
        do
        (cond
          ((or (< (- bomb-posx x) 1)
               (equal (list (- bomb-posx x) bomb-posy) mypos)
               (member (list (- bomb-posx x) bomb-posy) *kabe-list* :test #'equal)
               (member (list (- bomb-posx x) bomb-posy) *block-list* :test #'equal))
           (return))
          (t (push (list (- bomb-posx x) bomb-posy) *fire-list*))))
  (loop for y from 1 to bomb-p
        do
        (cond
          ((or (< (- bomb-posy y) 1)
               (equal (list bomb-posx (- bomb-posy y)) mypos)
               (member (list bomb-posx (- bomb-posy y)) *kabe-list* :test #'equal)
               (member (list bomb-posx (- bomb-posy y)) *block-list* :test #'equal))
           (return))
          (t (push (list bomb-posx (- bomb-posy y)) *fire-list*))))
  (loop for y from 1 to bomb-p
        do
        (cond
          ((or (> (+ bomb-posy y) 13)
               (equal (list bomb-posx (+ bomb-posy y)) mypos)
               (member (list bomb-posx (+ bomb-posy y)) *kabe-list* :test #'equal)
               (member (list bomb-posx (+ bomb-posy y)) *block-list* :test #'equal))
           (return))
          (t (push (list bomb-posx (+ bomb-posy y)) *fire-list*)))))
   
(defun make-fire-list2 (bomb-info mypos)
  (loop for i from 0 to (- (length bomb-info) 1)
        do
        (let* ((bomb-posx (cdr (second (assoc ':pos (nth i bomb-info)))))
               (bomb-posy (cdr (third (assoc ':pos (nth i bomb-info)))))
               ;;ボムパワー
               (bomb-p (cdr (assoc ':power (nth i bomb-info)))))
          (fire-list-dayo bomb-posx bomb-posy bomb-p mypos))))


(defun make-simfire-list (myposx myposy mybomb-p mypos)
  (push mypos *simfire-list*)
  (loop for x from 1 to mybomb-p
        do
        (cond
          ((or (> (+ myposx x) 13)
               (equal mypos (list (+ myposx x) myposy))
               (member (list (+ myposx x) myposy) *kabe-list* :test #'equal)
               (member (list (+ myposx x) myposy) *block-list* :test #'equal))
           (return))
          (t (push (list (+ myposx x) myposy) *simfire-list*))))
  (loop for x from 1 to mybomb-p
        do
        (cond
          ((or (< (- myposx x) 1)
               (equal (list (- myposx x) myposy) mypos)
               (member (list (- myposx x) myposy) *kabe-list* :test #'equal)
               (member (list (- myposx x) myposy) *block-list* :test #'equal))
           (return))
          (t (push (list (- myposx x) myposy) *simfire-list*))))
  (loop for y from 1 to mybomb-p
        do
        (cond
          ((or (< (- myposy y) 1)
               (equal (list myposx (- myposy y)) mypos)
               (member (list myposx (- myposy y)) *kabe-list* :test #'equal)
               (member (list myposx (- myposy y)) *block-list* :test #'equal))
           (return))
          (t (push (list myposx (- myposy y)) *simfire-list*))))
  (loop for y from 1 to mybomb-p
        do
        (cond
          ((or (> (+ myposy y) 13)
               (equal (list myposx (+ myposy y)) mypos)
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
      ((equal next-pos simbombpos) nil)
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
            (member next-pos *simfire-list* :test #'equal))
        nil)
      ((equal next-pos simbombpos) nil)
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
      ((equal next-pos simbombpos) nil)
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
            (member next-pos *simfire-list* :test #'equal))
        nil)
      ((equal next-pos simbombpos) nil)
      (t (push "LEFT" *move-list*)))))

(defun simmove? (myposx myposy simbombpos mypos)
  (simup? myposx myposy simbombpos mypos)
  (simdown? myposx myposy simbombpos mypos)
  (simright? myposx myposy simbombpos mypos)
  (simleft? myposx myposy simbombpos mypos)
  (if (not *move-list*)
      nil
      (nth (random (length *move-list*)) *move-list*)))

;;i=ループ回数
(defun 10move-s (myposx myposy i simbombpos mypos)
  (setf *move-list* nil)
  (let ((dir (simmove? myposx myposy simbombpos mypos)))
    (cond 
      ((>= 0 i) (if (member (list myposx myposy) *simfire-list* :test #'equal)
                    nil
                    t))
      ((equal "UP" dir)
       (10move-s myposx (- myposy 1) (decf i) simbombpos (list myposx (- myposy 1))))
      ((equal "DOWN" dir)
       (10move-s myposx (+ myposy 1) (decf i) simbombpos (list myposx (+ myposy 1))))
      ((equal "RIGHT" dir)
       (10move-s (+ myposx 1) myposy (decf i) simbombpos (list (+ myposx 1) myposy)))
      ((equal "LEFT" dir)
       (10move-s (- myposx 1) myposy (decf i) simbombpos (list (- myposx 1) myposy)))
      (t (if (member (list myposx myposy) *simfire-list* :test #'equal)
                    nil
                    t)))))


(defun sim10 (posx posy pos bomb-p)
  (let ((simbombpos pos))
    (make-simfire-list posx posy bomb-p pos)
    (cond ((godown? posx posy)
           (setf *hoge* nil)
           (loop for i from 0 to 5
                 do
                 (push (10move-s posx (+ posy 1) 6 simbombpos pos) *hoge*))
           (if (> (count t *hoge*) 2)
               (push "DOWN" *sin-move-list*))))
    (cond ((goup? posx posy)
           (setf *hoge* nil)
           (loop for i from 0 to 5
                 do
                 (push (10move-s posx (- posy 1) 6 simbombpos pos) *hoge*))
           (if (> (count t *hoge*) 2)
               (push "UP" *sin-move-list*))))
    (cond ((goright? posx posy)
           (setf *hoge* nil)
           (loop for i from 0 to 5
                 do
                 (push (10move-s (+ posx 1) posy 6 simbombpos pos) *hoge*))
           (if (> (count t *hoge*) 2)
               (push "RIGHT" *sin-move-list*))))
    (cond ((goleft? posx posy)
           (setf *hoge* nil)
           (loop for i from 0 to 5
                 do
                 (push (10move-s (- posx 1) posy 6 simbombpos pos) *hoge*))
           (if (> (count t *hoge*) 2)
               (push "LEFT" *sin-move-list*))))
    (if (null *sin-move-list*)
        (progn
          (setf *okanai* t)
          (move? posx posy pos))
        (nth (random (length *sin-move-list*)) *sin-move-list*))))


(defun hoge () 
  (princ "もげげ")
  (fresh-line)
  (let ((id (read-line)))
    (loop
      (let* ((mapdata (with-input-from-string (in (read-line)) (read-json in)))
             (my-info (nth (+ (parse-integer id) 1) (fourth mapdata)))
             (myposx (cdr (second (second my-info))))
             (myposy (cdr (third (second my-info))))
             (mypos (list myposx myposy))
             (mybomb-p (+ 1 (cdr (third my-info))))
             (mybomblimit (cdr (assoc ':setbomblimit my-info)))
             (bomb-info (cdr (assoc ':bombs mapdata))))
        (setf *kabe-list* (cdr (second mapdata))
              *block-list* (cdr (third mapdata))
              *fire-list* nil
              *bomb-pos-list* nil
              *move-list* nil
              *simfire-list* nil
              *okanai* nil
              *sin-move-list* nil)
        (make-bomb-pos-list bomb-info)
        (make-fire-list2 bomb-info mypos)
        (princ (concatenate 'string (if (= 0 mybomblimit)
                                        (sim10m myposx myposy mypos)
                                        (sim10 myposx myposy mypos mybomb-p))
                             "," 
                             (if *okanai*
                                 "false"
                                 "true")))
        (fresh-line)))))

(hoge)


