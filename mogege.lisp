(load (merge-pathnames "json-pasa.lisp" *load-truename*))
(load (merge-pathnames "ido.lisp" *load-truename*))
(setf *random-state* (make-random-state t))
;;;;AI?
(defparameter *kabe-list* nil)
(defparameter *block-list* nil)
(defparameter *fire-list* nil)
(defparameter *real-fire-list* nil)
(defparameter *bomb-pos-list* nil)
(defparameter *item-pos-list* nil)
(defparameter *simbombpos-list* nil)
(defparameter *item-path* nil)
(defparameter *safe-path* nil)
(defparameter *adjacent* nil)
(defparameter *simfire-list* nil)
(defparameter *cango-list* nil)
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
(defparameter *safe-list* nil)
(defparameter *canget-item-list* nil)
(defparameter *move1* nil)
(defparameter *move5* nil)
(defparameter *move7* nil)
(defparameter *move10* nil)
(defparameter *move-list* nil)
(defparameter *okanai* nil)
(defparameter *kari-kabe* nil)
(defparameter *kabe1* '((1 1) (2 1) (3 1) (4 1) (5 1) (6 1) (7 1) (8 1) (9 1) (10 1) (11 1) (12 1) (13 1) (13 2) (13 3) (13 4) (13 5) (13 6) (13 7) (13 8) (13 9) (13 10) (13 11) (13 12) (13 13) (12 13) (11 13) (10 13) (9 13) (8 13) (7 13) (6 13) (5 13) (4 13) (3 13) (2 13) (1 13) (1 12) (1 11) (1 10) (1 9) (1 8) (1 7) (1 6) (1 5) (1 4) (1 3) (1 2)))
(defparameter *kabe2* '((3 2) (5 2) (7 2) (9 2) (11 2) (12 3) (12 5) (12 7) (12 9) (12 11) (11 12) (9 12) (7 12) (5 12) (3 12) (2 11) (2 9) (2 7) (2 5) (2 3)))
(defparameter *kabe3* '((3 3) (4 3) (5 3) (6 3) (7 3) (8 3) (9 3) (10 3) (11 3) (11 4) (11 5) (11 6) (11 7) (11 8) (11 9) (11 10) (11 11) (10 11) (9 11) (8 11) (7 11) (6 11) (5 11) (4 11) (3 11) (3 10) (3 9) (3 8) (3 7) (3 6) (3 5) (3 4)))
(defparameter *kabe4* '((5 4) (7 4) (9 4) (10 5) (10 7) (10 9) (9 10) (7 10) (5 10) (4 9) (4 7) (4 5)))
;;(defparameter *add-kabe* '((1 1) (2 1) (3 1) (4 1) (5 1) (6 1) (7 1) (8 1) (9 1) (10 1) (11 1) (12 1) (13 1) (13 2) (13 3) (13 4) (13 5) (13 6) (13 7) (13 8) (13 9) (13 10) (13 11) (13 12) (13 13) (12 13) (11 13) (10 13) (9 13) (8 13) (7 13) (6 13) (5 13) (4 13) (3 13) (2 13) (1 13) (1 12) (1 11) (1 10) (1 9) (1 8) (1 7) (1 6) (1 5) (1 4) (1 3) (1 2) 
;;                           (3 2) (4 2) (5 2) (6 2) (7 2) (8 2) (9 2) (10 2) (11 2) (12 2) (12 3) (12 4) (12 5) (12 6) (12 7) (12 8) (12 9) (12 10) (12 11) (12 12) (11 12) (10 12) (9 12) (8 12) (7 12) (6 12) (5 12) (4 12) (3 12) (2 12) (2 11) (2 10) (2 9) (2 8) (2 7) (2 6) (2 5) (2 4) (2 3)
;;                           (3 3) (4 3) (5 3) (6 3) (7 3) (8 3) (9 3) (10 3) (11 3) (11 4) (11 5) (11 6) (11 7) (11 8) (11 9) (11 10) (11 11) (10 11) (9 11) (8 11) (7 11) (6 11) (5 11) (4 11) (3 11) (3 10) (3 9) (3 8) (3 7) (3 6) (3 5) (3 4)
;;                           (4 4) (5 4) (6 4) (7 4) (8 4) (9 4) (10 4) (10 5) (10 6) (10 7) (10 8) (10 9) (10 10) (9 10) (8 10) (7 10) (6 10) (5 10) (4 10) (4 9) (4 8) (4 7) (4 6) (4 5)))


;;ボム位置list
(defun make-bomb-pos-list (bomb-info)
  (loop for i from 0 to (- (length bomb-info) 1)
        do
        (let ((bomb-pos (list (cdr (second (assoc ':pos (nth i bomb-info))))
                              (cdr (third (assoc ':pos (nth i bomb-info)))))))
          (push bomb-pos *bomb-pos-list*))))


;;火の予想位置リスト
(defun fire-list-dayo (bomb-posx bomb-posy bomb-p bomb-timer)
  (if (and (assoc (list bomb-posx bomb-posy) *fire-list* :test #'equal)
           (> bomb-timer (cdr (assoc (list bomb-posx bomb-posy) *fire-list* :test #'equal))))
      (setf bomb-timer (cdr (assoc (list bomb-posx bomb-posy) *fire-list* :test #'equal))))
  (push (cons (list bomb-posx bomb-posy) bomb-timer) *fire-list*)
  (loop for x from 1 to bomb-p
        do
        (let ((fire-area (list (+ bomb-posx x) bomb-posy)))
          (cond
            ((or (member fire-area *kabe-list* :test #'equal)
                 (member fire-area *block-list* :test #'equal))
             (return))
            ;;((assoc fire-area *fire-list* :test #'equal) nil)
            (t (push (cons fire-area bomb-timer) *fire-list*)))))
  (loop for x from 1 to bomb-p
        do
        (let ((fire-area (list (- bomb-posx x) bomb-posy)))
          (cond
            ((or (member fire-area *kabe-list* :test #'equal)
                 (member fire-area *block-list* :test #'equal))
             (return))
            ;;((assoc fire-area *fire-list* :test #'equal) nil)
          (t (push (cons fire-area bomb-timer) *fire-list*)))))
  (loop for y from 1 to bomb-p
        do
        (let ((fire-area (list bomb-posx (- bomb-posy y))))
          (cond
            ((or (member fire-area *kabe-list* :test #'equal)
                 (member fire-area *block-list* :test #'equal))
             (return))
            ;;((assoc fire-area *fire-list* :test #'equal) nil)
            (t (push (cons fire-area bomb-timer) *fire-list*)))))
  (loop for y from 1 to bomb-p
        do
        (let ((fire-area (list bomb-posx (+ bomb-posy y))))
          (cond
            ((or (member fire-area *kabe-list* :test #'equal)
                 (member fire-area *block-list* :test #'equal))
             (return))
            ;;((assoc fire-area *fire-list* :test #'equal) nil)
            (t (push (cons fire-area bomb-timer) *fire-list*))))))

;;おいてある爆弾の火のリストを作る
(defun make-fire-list2 (bomb-info)
  (loop for i from 0 to (- (length bomb-info) 1)
        do
        (let* ((bomb-posx (cdr (second (assoc ':pos (nth i bomb-info)))))
               (bomb-posy (cdr (third (assoc ':pos (nth i bomb-info)))))
               ;;ボムタイマー
               (bomb-timer (cdr (assoc ':timer (nth i bomb-info))))
               ;;ボムパワー
               (bomb-p (cdr (assoc ':power (nth i bomb-info)))))
          (fire-list-dayo bomb-posx bomb-posy bomb-p bomb-timer)))
  (setf *fire-list* (sort *fire-list* #'< :key #'cdr)))

(defun make-fire-list3 (bomb-info)
  (if (null bomb-info)
      (setf *fire-list* (sort *fire-list* #'< :key #'cdr))
      (let* ((bomb (car bomb-info))
             (bomb-posx (cdr (second (car bomb))))
             (bomb-posy (cdr (third (car bomb))))
             (bomb-timer (cdr (assoc ':timer bomb)))
             (bomb-p (cdr (assoc ':power bomb))))
        (fire-list-dayo bomb-posx bomb-posy bomb-p bomb-timer)
        (make-fire-list3 (cdr bomb-info)))))

;;今いる場所に爆弾をおいた場合の火のリストを作る
(defun make-simfire-list (myposx myposy mybomb-p)
  ;;(push mypos *simfire-list*)
  (loop for x from 0 to mybomb-p
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


;;盤面にあるアイテムリスト
(defun make-item-pos-list (item-info)
  (setf *item-pos-list* nil)
  (loop for i from 0 to (- (length item-info) 1)
        do
        (let ((item-pos (list (cdr (second (assoc ':pos (nth i item-info))))
                              (cdr (third (assoc ':pos (nth i item-info)))))))
          (push item-pos *item-pos-list*))))

;;現在の場所から行くことのできる場所一覧
(defun make-cango-list (posx posy)
  (if (not (member (list posx posy) *cango-list* :test #'equal))
      (push (list posx posy) *cango-list*))
  (let ((posx-1 (list (- posx 1) posy))
        (posy+1 (list posx (+ posy 1)))
        (posx+1 (list (+ posx 1) posy))
        (posy-1 (list posx (- posy 1))))
    (if (and
          (not (member posx-1 *cango-list* :test #'equal))
          (not (member posx-1 *kabe-list* :test #'equal))
          (not (member posx-1 *block-list* :test #'equal))
          (not (member posx-1 *bomb-pos-list* :test #'equal)))
        (progn
          (push posx-1 *cango-list*)
          (make-cango-list (- posx 1) posy)))
    (if (and
          (not (member posy+1 *cango-list* :test #'equal))
          (not (member posy+1 *kabe-list* :test #'equal))
          (not (member posy+1 *block-list* :test #'equal))
          (not (member posy+1 *bomb-pos-list* :test #'equal)))
        (progn
          (push posy+1 *cango-list*)
          (make-cango-list posx (+ posy 1))))
    (if (and
          (not (member posx+1 *cango-list* :test #'equal))
          (not (member posx+1 *kabe-list* :test #'equal))
          (not (member posx+1 *block-list* :test #'equal))
          (not (member posx+1 *bomb-pos-list* :test #'equal)))
        (progn
          (push posx+1 *cango-list*)
          (make-cango-list (+ posx 1) posy)))
    (if (and
          (not (member posy-1 *cango-list* :test #'equal))
          (not (member posy-1 *kabe-list* :test #'equal))
          (not (member posy-1 *block-list* :test #'equal))
          (not (member posy-1 *bomb-pos-list* :test #'equal)))
        (progn
          (push posy-1 *cango-list*)
          (make-cango-list posx (- posy 1))))))

(defun make-safe-list (pos fire-list simfire-list)
  (if (or (null *safe-list*)
          (and (null fire-list)
               (null simfire-list)))
      nil
      (progn
        (cond
          ((and fire-list simfire-list)
           (setf *safe-list* (remove (caar fire-list) *safe-list* :test #'equal))
           (setf *safe-list* (remove (car simfire-list) *safe-list* :test #'equal))
           (make-safe-list pos (cdr fire-list) (cdr simfire-list)))
          ((and fire-list (null simfire-list))
           (setf *safe-list* (remove (caar fire-list) *safe-list* :test #'equal))
           (make-safe-list pos (cdr fire-list) simfire-list))
          ((and (null fire-list) simfire-list)
           (setf *safe-list* (remove (car simfire-list) *safe-list* :test #'equal))
           (make-safe-list pos fire-list (cdr simfire-list)))))))

(defun make-safe-list-b (pos fire-list)
  (if (or (null *safe-list*)
          (null fire-list))
      (setf *safe-list* (remove pos *safe-list* :test #'equal))
      (progn
        (setf *safe-list* (remove (caar fire-list) *safe-list* :test #'equal))
        (make-safe-list-b pos (cdr fire-list)))))
    
               

(defun make-canget-item-list (item-pos-list)
  (if (null item-pos-list)
      nil
      (cond 
        ((find (car item-pos-list) *cango-list* :test #'equal)
         (push (find (car item-pos-list) *cango-list* :test #'equal) *canget-item-list*)
         (make-canget-item-list (cdr item-pos-list)))
        (t (make-canget-item-list (cdr item-pos-list))))))

;;アイテムの方へ移動する
(defun item-to-ido ()
  (let* ((now-pos (first *item-path*))
         (next-pos (second *item-path*))
         (dir (mapcar #'- next-pos now-pos)))
    (cond
      ((equal '(0 1) dir) "DOWN")
      ((equal '(1 0) dir) "RIGHT")
      ((equal '(-1 0) dir) "LEFT")
      ((equal '(0 -1) dir) "UP"))))
;;一番近いセーフ位置へ移動
(defun safe-to-ido ()
  (let* ((rand-path (nth (random (length *safe-path*)) *safe-path*))
         (now-pos (first rand-path))
         (next-pos (second rand-path))
         (dir (mapcar #'- next-pos now-pos)))
    (cond
      ((equal '(0 1) dir) "DOWN")
      ((equal '(1 0) dir) "RIGHT")
      ((equal '(-1 0) dir) "LEFT")
      ((equal '(0 -1) dir) "UP"))))

;; キューの定義
(defstruct Queue (front nil) (rear nil))

;; データを入れる
(defun enqueue (queue item)
  (let ((new-cell (list item)))
    (if (Queue-front queue)
      ; 最終セルを書き換える
      (setf (cdr (Queue-rear queue)) new-cell)
      ; キューは空の状態
      (setf (Queue-front queue) new-cell))
    (setf (Queue-rear queue) new-cell)))

;; データを取り出す
(defun dequeue (queue)
  (if (Queue-front queue)
      (prog1
        (pop (Queue-front queue))
        (unless (Queue-front queue)
          ; キューは空になった
          (setf (Queue-rear queue) nil)))))


(defun rensou-list (pos)
  (let ((posx (first pos))
        (posy (second pos))
        (kari nil))
    (if (goup? pos)
        (push (list posx (- posy 1)) kari))
    (if (godown? pos)
        (push (list posx (+ posy 1)) kari))
    (if (goright? pos)
        (push (list (+ posx 1) posy) kari))
    (if (goleft? pos)
        (push (list (- posx 1) posy) kari))
    (push (cons pos kari) *adjacent*)))

(defun make-cango-rensou-list (cango-list)
  (if (null cango-list)
      nil
      (progn 
        (rensou-list (car cango-list))
        (make-cango-rensou-list (cdr cango-list)))))
             


;; ***** 幅優先探索 *****
(defun breadth-search (goal start)
  (let ((queue (make-Queue)) path)
    ;; 出発点をキューにセット
    (enqueue queue (list start))
    (loop while (setf path (dequeue queue))
          do
          (dolist (node (assoc (car path) *adjacent* :test #'equal))
            (if (equal goal node)
                ;; 経路を表示する
                (return (setf *item-path* (reverse (cons node path))))
                (unless (member node path :test #'equal)
                  ;; 経路をキューに追加
                  (enqueue queue (cons node path))))))))

;; 経路の探索（反復深化）
(defun search-item-path (limit goal path)
  (if (= limit (length path))
      (if (equal goal (car path))
          ; 経路を表示
          (setf *item-path* (reverse path)))
      (dolist (node (assoc (car path) *adjacent* :test #'equal))
        (unless (member node path :test #'equal)
          ; 再帰呼び出し
          (search-item-path limit goal (cons node path))))))

;;アイテムまでの道順生成
(defun make-item-path (pos)
  (dotimes (x 10)
    (if *item-path*
        (return)
        (search-item-path (1+ x) (car *canget-item-list*) (list pos)))))

(defun search-safe-path (limit goal path)
  (if (= limit (length path))
      (if (equal goal (car path))
          ; 経路を表示
          (push (reverse path) *safe-path*))
      (dolist (node (assoc (car path) *adjacent* :test #'equal))
        (unless (member node path :test #'equal)
          ; 再帰呼び出し
          (search-safe-path limit goal (cons node path))))))

;;アイテムまでの道順生成
(defun make-safe-path (pos safe-list)
  (if (null safe-list)
      (progn
        (setf *safe-path* (sort *safe-path* #'< :key #'length))
        (setf *safe-path*
              (remove-if #'(lambda (x) 
                            (< (length (car *safe-path*)) (length x)))
                         *safe-path*)))
      (progn
        (dotimes (x 10)
          (search-safe-path (1+ x) (car safe-list) (list pos)))
        (make-safe-path pos (cdr safe-list)))))


(defun item-toruzo (item-info myposx myposy)
  (make-item-pos-list item-info)
  (make-cango-list myposx myposy)
  (make-cango-rensou-list *cango-list*)
  (make-canget-item-list *item-pos-list*))

;;敵の情報取得
(defun set-enemy-info (id-list mapdata)
  (setf *enemy0posx* (cdr (second (assoc ':pos (nth (first id-list) (cdr (assoc ':players mapdata))))))
        *enemy0posy* (cdr (third (assoc ':pos (nth (first id-list) (cdr (assoc ':players mapdata)))))))
  (setf *enemy0pos* (list *enemy0posx* *enemy0posy*))
  (setf *enemy0bombp* (cdr (assoc ':power (nth (first id-list) (cdr (assoc ':players mapdata))))))
  (setf *enemy1posx* (cdr (second (assoc ':pos (nth (second id-list) (cdr (assoc ':players mapdata))))))
        *enemy1posy* (cdr (third (assoc ':pos (nth (second id-list) (cdr (assoc ':players mapdata)))))))
  (setf *enemy1pos* (list *enemy1posx* *enemy1posy*))
  (setf *enemy1bombp* (cdr (assoc ':power (nth (second id-list) (cdr (assoc ':players mapdata))))))
  (setf *enemy2posx* (cdr (second (assoc ':pos (nth (third id-list) (cdr (assoc ':players mapdata))))))
        *enemy2posy* (cdr (third (assoc ':pos (nth (third id-list) (cdr (assoc ':players mapdata)))))))
  (setf *enemy2pos* (list *enemy2posx* *enemy2posy*))
  (setf *enemy2bombp* (cdr (assoc ':power (nth (third id-list) (cdr (assoc ':players mapdata)))))))


(defun goup? (pos)
  (let* ((next-pos (mapcar #'- pos '(0 1))))
    (cond
      ((< (cadr next-pos) 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *kari-kabe*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ;;((member next-pos *real-fire-list* :test #'equal) nil)
      ((and (not (assoc pos *fire-list* :test #'equal))
            (assoc next-pos *fire-list* :test #'equal)) nil)
      ((and (assoc pos *fire-list* :test #'equal)
            (assoc next-pos *fire-list* :test #'equal)
            (> (- (cdr (assoc pos *fire-list* :test #'equal)) 
                  (cdr (assoc next-pos *fire-list* :test #'equal))) 1)) nil)
      ((and (assoc next-pos *fire-list* :test #'equal)
            (>= 1 (cdr (assoc next-pos *fire-list* :test #'equal)))) nil)
      (t t))))

(defun godown? (pos)
  (let* ((next-pos (mapcar #'+ pos '(0 1))))
    (cond
      ((> (cadr next-pos) 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *kari-kabe*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ;;((member next-pos *real-fire-list* :test #'equal) nil)
      ((and (not (assoc pos *fire-list* :test #'equal))
            (assoc next-pos *fire-list* :test #'equal)) nil)
      ((and (assoc pos *fire-list* :test #'equal)
            (assoc next-pos *fire-list* :test #'equal)
            (> (- (cdr (assoc pos *fire-list* :test #'equal)) 
                  (cdr (assoc next-pos *fire-list* :test #'equal))) 1)) nil)
      ((and (assoc next-pos *fire-list* :test #'equal)
            (>= 1 (cdr (assoc next-pos *fire-list* :test #'equal)))) nil)
      (t t))))

(defun goright? (pos)
  (let* ((next-pos (mapcar #'+ pos '(1 0))))
    (cond
      ((> (car next-pos) 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *kari-kabe*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ;;((member next-pos *real-fire-list* :test #'equal) nil)
      ((and (not (assoc pos *fire-list* :test #'equal))
            (assoc next-pos *fire-list* :test #'equal)) nil)
      ((and (assoc pos *fire-list* :test #'equal)
            (assoc next-pos *fire-list* :test #'equal)
            (> (- (cdr (assoc pos *fire-list* :test #'equal)) 
                  (cdr (assoc next-pos *fire-list* :test #'equal))) 1)) nil)
      ((and (assoc next-pos *fire-list* :test #'equal)
            (>= 1 (cdr (assoc next-pos *fire-list* :test #'equal)))) nil)
      (t t))))

(defun goleft? (pos)
  (let* ((next-pos (mapcar #'- pos '(1 0))))
    (cond
      ((< (car next-pos) 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *kari-kabe*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ;;((member next-pos *real-fire-list* :test #'equal) nil)
      ((and (not (assoc pos *fire-list* :test #'equal))
            (assoc next-pos *fire-list* :test #'equal)) nil)
      ((and (assoc pos *fire-list* :test #'equal)
            (assoc next-pos *fire-list* :test #'equal)
            (> (- (cdr (assoc pos *fire-list* :test #'equal)) 
                  (cdr (assoc next-pos *fire-list* :test #'equal))) 1)) nil)
      ((and (assoc next-pos *fire-list* :test #'equal)
            (>= 1 (cdr (assoc next-pos *fire-list* :test #'equal)))) nil)
      (t t))))

(defun simup? (now-pos karibomb-pos fire-list)
  (let* ((next-pos (mapcar #'- now-pos '(0 1))))
    (cond
      ((< (cadr next-pos) 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ;;((member next-pos *real-fire-list* :test #'equal) nil)
      ((member next-pos karibomb-pos    :test #'equal) nil)
      
      ((and (not (assoc now-pos fire-list :test #'equal))
            (assoc next-pos fire-list :test #'equal)) nil)
      ((and (assoc next-pos fire-list :test #'equal)
            (>= 1 (cdr (assoc next-pos fire-list :test #'equal))))
       nil)
      
      ((and (not (assoc now-pos fire-list :test #'equal))
            (not (assoc next-pos fire-list :test #'equal)))
       (push "UP" *move7*))
      ((and (not (member now-pos *simfire-list* :test #'equal))
            (not (assoc next-pos fire-list :test #'equal))
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "UP" *move7*))
      
      ((and (assoc now-pos fire-list :test #'equal)
            (not (assoc next-pos fire-list :test #'equal)))
       (push "UP" *move10*))
      ((and (member now-pos *simfire-list* :test #'equal)
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "UP" *move7*))
      
      ((and (assoc now-pos fire-list :test #'equal)
            (assoc next-pos fire-list :test #'equal)
            (= 1 (- (cdr (assoc now-pos fire-list :test #'equal))
                    (cdr (assoc next-pos fire-list :test #'equal)))))
       (push "UP" *move5*))
      ((and (assoc now-pos fire-list :test #'equal)
            (not (assoc next-pos fire-list :test #'equal))
            (member next-pos *simfire-list* :test #'equal))
       (push "UP" *move5*))
            

      ((and (assoc now-pos fire-list :test #'equal)
            (assoc next-pos fire-list :test #'equal)
            (>= (- (cdr (assoc next-pos fire-list :test #'equal))
                   (cdr (assoc now-pos fire-list :test #'equal))) 2))
       (push "UP" *move1*))
      ;;((and (member now-pos *simfire-list* :test #'equal)
      ;;      (member next-pos *simfire-list* :test #'equal))
      ;; (push "UP" *move1*))
      (t nil))))

(defun simdown? (now-pos karibomb-pos fire-list)
  (let* ((next-pos (mapcar #'+ now-pos '(0 1))))
    (cond
      ((> (cadr next-pos) 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ;;((member next-pos *real-fire-list* :test #'equal) nil)
      ((member next-pos karibomb-pos    :test #'equal) nil)
      
      ((and (not (assoc now-pos fire-list :test #'equal))
            (assoc next-pos fire-list :test #'equal)) nil)
      ((and (assoc next-pos fire-list :test #'equal)
            (>= 1 (cdr (assoc next-pos fire-list :test #'equal))))
       nil)
      
      ((and (not (assoc now-pos fire-list :test #'equal))
            (not (assoc next-pos fire-list :test #'equal)))
       (push "DOWN" *move7*))
      ((and (not (member now-pos *simfire-list* :test #'equal))
            (not (assoc next-pos fire-list :test #'equal))
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "DOWN" *move7*))
      
      ((and (assoc now-pos fire-list :test #'equal)
            (not (assoc next-pos fire-list :test #'equal)))
       (push "DOWN" *move10*))
      ((and (member now-pos *simfire-list* :test #'equal)
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "DOWN" *move7*))
      
      ((and (assoc now-pos fire-list :test #'equal)
            (assoc next-pos fire-list :test #'equal)
            (= 1 (- (cdr (assoc now-pos fire-list :test #'equal))
                    (cdr (assoc next-pos fire-list :test #'equal)))))
       (push "DOWN" *move5*))
      ((and (assoc now-pos fire-list :test #'equal)
            (not (assoc next-pos fire-list :test #'equal))
            (member next-pos *simfire-list* :test #'equal))
       (push "DOWN" *move5*))
      

      ((and (assoc now-pos fire-list :test #'equal)
            (assoc next-pos fire-list :test #'equal)
            (>= (- (cdr (assoc next-pos fire-list :test #'equal))
                   (cdr (assoc now-pos fire-list :test #'equal))) 2))
       (push "DOWN" *move1*))
      ;;((and (member now-pos *simfire-list* :test #'equal)
      ;;      (member next-pos *simfire-list* :test #'equal))
      ;; (push "DOWN" *move1*))
      (t nil))))

(defun simright? (now-pos karibomb-pos fire-list)
  (let* ((next-pos (mapcar #'+ now-pos '(1 0))))
    (cond
      ((> (car next-pos) 13) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ;;((member next-pos *real-fire-list* :test #'equal) nil)
      ((member next-pos karibomb-pos    :test #'equal) nil)
      
      ((and (not (assoc now-pos fire-list :test #'equal))
            (assoc next-pos fire-list :test #'equal)) nil)
      ((and (assoc next-pos fire-list :test #'equal)
            (>= 1 (cdr (assoc next-pos fire-list :test #'equal))))
       nil)
      
      ((and (not (assoc now-pos fire-list :test #'equal))
            (not (assoc next-pos fire-list :test #'equal)))
       (push "RIGHT" *move7*))
      ((and (not (member now-pos *simfire-list* :test #'equal))
            (not (assoc next-pos fire-list :test #'equal))
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "RIGHT" *move7*))
      
      ((and (assoc now-pos fire-list :test #'equal)
            (not (assoc next-pos fire-list :test #'equal)))
       (push "RIGHT" *move10*))
      ((and (member now-pos *simfire-list* :test #'equal)
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "RIGHT" *move7*))
      
      ((and (assoc now-pos fire-list :test #'equal)
            (assoc next-pos fire-list :test #'equal)
            (= 1 (- (cdr (assoc now-pos fire-list :test #'equal))
                    (cdr (assoc next-pos fire-list :test #'equal)))))
       (push "RIGHT" *move5*))
      ((and (assoc now-pos fire-list :test #'equal)
            (not (assoc next-pos fire-list :test #'equal))
            (member next-pos *simfire-list* :test #'equal))
       (push "RIGHT" *move5*))
            

      ((and (assoc now-pos fire-list :test #'equal)
            (assoc next-pos fire-list :test #'equal)
            (>= (- (cdr (assoc next-pos fire-list :test #'equal))
                   (cdr (assoc now-pos fire-list :test #'equal))) 2))
       (push "RIGHT" *move1*))
      ;;((and (member now-pos *simfire-list* :test #'equal)
      ;;      (member next-pos *simfire-list* :test #'equal))
      ;; (push "RIGHT" *move1*))
      (t nil))))

(defun simleft? (now-pos karibomb-pos fire-list)
  (let* ((next-pos (mapcar #'- now-pos '(1 0))))
    (cond
      ((< (car next-pos) 1) nil)
      ((member next-pos *kabe-list*     :test #'equal) nil)
      ((member next-pos *block-list*    :test #'equal) nil)
      ((member next-pos *bomb-pos-list* :test #'equal) nil)
      ;;((member next-pos *real-fire-list* :test #'equal) nil)
      ((member next-pos karibomb-pos    :test #'equal) nil)
      
      ((and (not (assoc now-pos fire-list :test #'equal))
            (assoc next-pos fire-list :test #'equal)) nil)
      
      ((and (assoc next-pos fire-list :test #'equal)
            (>= 1 (cdr (assoc next-pos fire-list :test #'equal))))
       nil)
      
      ((and (not (assoc now-pos fire-list :test #'equal))
            (not (assoc next-pos fire-list :test #'equal)))
       (push "LEFT" *move7*))
      ((and (not (member now-pos *simfire-list* :test #'equal))
            (not (assoc next-pos fire-list :test #'equal))
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "LEFT" *move7*))
      
      ((and (assoc now-pos fire-list :test #'equal)
            (not (assoc next-pos fire-list :test #'equal)))
       (push "LEFT" *move10*))
      ((and (member now-pos *simfire-list* :test #'equal)
            (not (member next-pos *simfire-list* :test #'equal)))
       (push "LEFT" *move7*))
      
      ((and (assoc now-pos fire-list :test #'equal)
            (assoc next-pos fire-list :test #'equal)
            (= 1 (- (cdr (assoc now-pos fire-list :test #'equal))
                    (cdr (assoc next-pos fire-list :test #'equal)))))
       (push "LEFT" *move5*))
      
      ((and (assoc now-pos fire-list :test #'equal)
            (not (assoc next-pos fire-list :test #'equal))
            (member next-pos *simfire-list* :test #'equal))
       (push "LEFT" *move5*))
            

      ((and (assoc now-pos fire-list :test #'equal)
            (assoc next-pos fire-list :test #'equal)
            (>= (- (cdr (assoc next-pos fire-list :test #'equal))
                   (cdr (assoc now-pos fire-list :test #'equal))) 2))
       (push "LEFT" *move1*))
      ;;((and (member now-pos *simfire-list* :test #'equal)
      ;;      (member next-pos *simfire-list* :test #'equal))
      ;; (push "LEFT" *move1*))
      (t nil))))

;;次に行けるマスをさがす
(defun simra (pos mybomb-p mybombcount mybomblimit)
  (setf *safe-list* (copy-list *cango-list*)
        *simfire-list* nil
        *safe-path* nil)
  (let ((pos-u (mapcar #'- pos '(0 1)))
        (pos-d (mapcar #'+ pos '(0 1)))
        (pos-r (mapcar #'+ pos '(1 0)))
        (pos-l (mapcar #'- pos '(1 0)))
        (karibomb-pos (list pos *enemy0pos* *enemy1pos* *enemy2pos*))
        (fire-list *fire-list*)
        (move-list nil))
    (make-simfire-list (car pos) (cadr pos) mybomb-p)
    (make-safe-list pos *fire-list* *simfire-list*)
    (ido-yosou pos)
    (if (member pos *bomb-pos-list* :test #'equal)
        (setf *okanai* t))
    (make-safe-path pos *safe-list*)
    (cond
      ((and (assoc pos fire-list :test #'equal)
            *safe-path*
            (> (length (car *safe-path*)) (cdr (assoc pos fire-list :test #'equal))))
       (setf *okanai* t)
       (okanai-simra pos))
      ((and *safe-path*
            (tonari-enemy pos))
       (safe-to-ido))
      ((= mybomblimit mybombcount)
       (okanai-simra pos))
      (*safe-path*
        (safe-to-ido))
      (t (setf *okanai* t)
         (okanai-simra pos)))))


(defun okanai-simra (pos)
  (setf *safe-list* (copy-list *cango-list*)
        *simfire-list* nil
        *safe-path* nil
        *okanai* t)
  (let ((simbombpos (list *enemy1pos* *enemy2pos* *enemy0pos*))
        (fire-list *fire-list*))
    (make-safe-list-b pos *fire-list*)
    (make-safe-path pos *safe-list*)
    (cond
      (*safe-path*
        (safe-to-ido))
      (t (simmove? pos simbombpos fire-list)))))
;;隣に敵がいたらボムを置く
#|
(defun tonari-enemy (mypos)
  (let ((hoge (mapcar #'- mypos *enemy0pos*))
        (hage (mapcar #'- mypos *enemy1pos*))
        (hige (mapcar #'- mypos *enemy2pos*))
        (moge (mapcar #'- mypos *enemy3pos*)))
    (cond
      ((or (equal hoge '(0 1))
           (equal hoge '(0 -1))
           (equal hoge '(1 0))
           (equal hoge '(-1 0))) t)
      ((or (equal hage '(0 1))
           (equal hage '(0 -1))
           (equal hage '(1 0))
           (equal hage '(-1 0))) t)
      ((or (equal hige '(0 1))
           (equal hige '(0 -1))
           (equal hige '(1 0))
           (equal hige '(-1 0))) t)
      ((or (equal moge '(0 1))
           (equal moge '(0 -1))
           (equal moge '(1 0))
           (equal moge '(-1 0))) t))))
|#
(defun tonari-enemy (mypos)
  (some
    (lambda (enemy)
      (let ((diff (mapcar #'- mypos enemy)))
        (find diff '((0 1) (0 -1) (1 0) (-1 0)) :test #'equal)))
    (list *enemy0pos* *enemy1pos* *enemy2pos*)))
;;予想した移動先の移動先が一つ以下だった場合、敵の移動先も予想する
;;敵の予想移動先が自分の予想移動先と隣り合ったらnil
(defun enemy-ido-yosou2 (mynext-pos)
  (some 
    (lambda (enemy)
      (let* ((epos-u (mapcar #'- enemy '(0 1)))
             (diffu (mapcar #'- mynext-pos epos-u))
             (epos-d (mapcar #'+ enemy '(0 1)))
             (diffd (mapcar #'- mynext-pos epos-d))
             (epos-r (mapcar #'+ enemy '(1 0)))
             (diffr (mapcar #'- mynext-pos epos-r))
             (epos-l (mapcar #'- enemy '(1 0)))
             (diffl (mapcar #'- mynext-pos epos-l))
             (diff (mapcar #'- mynext-pos enemy)))
      (cond
        ((and (kariup? enemy enemy)        
              (find diffu '((0 1) (0 -1) (1 0) (-1 0)) :test #'equal)) t)
        ((and (karidown? enemy enemy)
              (find diffd '((0 1) (0 -1) (1 0) (-1 0)) :test #'equal)) t)
        ((and (kariright? enemy enemy)
              (find diffr '((0 1) (0 -1) (1 0) (-1 0)) :test #'equal)) t)
        ((and (karileft? enemy enemy)
              (find diffl '((0 1) (0 -1) (1 0) (-1 0)) :test #'equal)) t)
        ((find diff '((0 1) (0 -1) (1 0) (-1 0)) :test #'equal) t))))
    (list *enemy0pos* *enemy1pos* *enemy2pos*)))

;;自分の移動先を予想
(defun ido-yosou (mypos)
  (let ((mypos-u (mapcar #'- mypos '(0 1)))
        (mypos-d (mapcar #'+ mypos '(0 1)))
        (mypos-r (mapcar #'+ mypos '(1 0)))
        (mypos-l (mapcar #'- mypos '(1 0))))
    (if (goup? mypos)
        (if (ido-ato mypos-u mypos)
            (setf *safe-list* (remove mypos-u *safe-list* :test #'equal))))
    (if (godown? mypos)
        (if (ido-ato mypos-d mypos)
            (setf *safe-list* (remove mypos-d *safe-list* :test #'equal))))
    (if (goright? mypos)
        (if (ido-ato mypos-r mypos)
            (setf *safe-list* (remove mypos-r *safe-list* :test #'equal))))
    (if (goleft? mypos)
        (if (ido-ato mypos-l mypos)
            (setf *safe-list* (remove mypos-l *safe-list* :test #'equal))))))
;;予想した移動先の移動先が一つ以下か調べる
(defun ido-ato (mynext-pos mypos)
  (let ((hoge nil))
    (if (kariup? mynext-pos mypos)
        (push t hoge) (push nil hoge))
    (if (karidown? mynext-pos mypos)
        (push t hoge) (push nil hoge))
    (if (kariright? mynext-pos mypos)
        (push t hoge) (push nil hoge))
    (if (karileft? mynext-pos mypos)
        (push t hoge) (push nil hoge))
    (if (>= 1 (count t hoge))
        (enemy-ido-yosou2 mynext-pos)
        nil)))

  
;;一つ先の場所からさらに上下左右どこへ行けるかさがす
(defun simmove? (now-pos simbombpos fire-list)
  (setf *move1* nil
        *move10* nil
        *move5* nil
        *move7* nil)
  (simup? now-pos simbombpos fire-list)
  (simdown? now-pos simbombpos fire-list)
  (simright? now-pos simbombpos fire-list)
  (simleft? now-pos simbombpos fire-list)
  (cond (*move10*
          (nth (random (length *move10*)) *move10*))
        (*move7*
          (nth (random (length *move7*)) *move7*))
        (*move5*
          (nth (random (length *move5*)) *move5*))
        (*move1*
          (nth (random (length *move1*)) *move1*))
        (t "STAY")))
  

(defun hoge () 
  (princ "もげげエターナル")
  (fresh-line)
  (let ((id (read-line)))
    (loop
      (let* ((mapdata (with-input-from-string (in (read-line)) (read-json in)))
             (id-list (list 0 1 2 3))
             (my-info (nth (parse-integer id) (cdr (assoc ':players mapdata))))
             (myposx (cdr (second (second my-info))))
             (myposy (cdr (third (second my-info))))
             (mypos (list myposx myposy))
             (mybomb-p (+ 1 (cdr (third my-info))))
             (mybomblimit (cdr (assoc ':setbomblimit my-info)))
             (mybombcount (cdr (assoc ':setbombcount my-info)))
             (turn-n (cdr (assoc ':turn mapdata)))
             (bomb-info (cdr (assoc ':bombs mapdata)))
             (item-info (cdr (assoc ':items mapdata))))
        (setf *kabe-list* (cdr (second mapdata))
              *block-list* (cdr (third mapdata))
              *real-fire-list* (cdr (assoc ':fires mapdata))
              *fire-list* nil
              *bomb-pos-list* nil
              *simfire-list* nil
              *okanai* nil
              ;;*sin-move-list* nil
              *canget-item-list* nil
              *cango-list* nil
              *item-path* nil
              *adjacent* nil
              *safe-list* nil
              *kari-kabe* nil)
        (if (> mybomblimit 2)
            (setf mybomblimit (- mybomblimit 1)))
        (cond
          ((>= turn-n 503) nil)
          ((and (>= turn-n 469) (not (member mypos *kabe4* :test #'equal)))
           (setf *kari-kabe* *kabe4*))
          ((and (>= turn-n 437) (not (member mypos *kabe3* :test #'equal)))
           (setf *kari-kabe* *kabe3*))
          ((and (>= turn-n 397) (not (member mypos *kabe2* :test #'equal)))
           (setf *kari-kabe* *kabe2*))
          ((and (>= turn-n 350) (not (member mypos *kabe1* :test #'equal)))
           (setf *kari-kabe* *kabe1*)))
        (setf id-list (remove (parse-integer id) id-list))
        (set-enemy-info id-list mapdata)
        (make-bomb-pos-list bomb-info)
        (make-fire-list3 bomb-info)
        (item-toruzo item-info myposx myposy)
        (make-item-path mypos)
        (cond 
          (*item-path*
            (princ (concatenate 'string (item-to-ido) "," "false"))
            (fresh-line))
          (t
            (princ (concatenate 'string ;;(if (= mybombcount mybomblimit)
                                            ;;(okanai-simra mypos)
                                            (simra mypos mybomb-p mybombcount mybomblimit)
                                "," 
                                (if *okanai*
                                    "false"
                                    "true")))
            (fresh-line)))))))

(hoge)


