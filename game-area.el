;;; game-area.el
;;;

;;   0 1 2 3 4
;; 0|@|.|.|.|.|
;; 1|.|.|@|.|.|
;; 2|.|.|.|.|@|
;; 3|.|.|.|@|.|
;; 4|.|@|.|.|.|

;; @ -- モンスターの位置

(defun make-area ()
  (setq place (make-vector 5 nil))
  (aset place 0 (make-vector 5 nil))
  (aset place 1 (make-vector 5 nil))
  (aset place 2 (make-vector 5 nil))
  (aset place 3 (make-vector 5 nil))
  (aset place 4 (make-vector 5 nil))
  (aset (aref place 0) 0 "monster")
  (aset (aref place 1) 2 "monster")
  (aset (aref place 2) 4 "monster")
  (aset (aref place 3) 3 "monster")
  (aset (aref place 4) 1 "monster")
  )

(make-area)
"monster"

(aref place 0)
["monster" nil nil nil nil]
(aref place 1)
[nil nil "monster" nil nil]
(aref place 2)
[nil nil nil nil "monster"]
(aref place 3)
[nil nil nil "monster" nil]
(aref place 4)
[nil "monster" nil nil nil]

(let ((x (random 5))
      (y (random 5)))
  (aref (aref place y) x))
"monster"

nil









(setq place (make-vector 5 nil))
[nil nil nil nil nil]

(aset place 0 "Osaka")
"Osaka"
(aset place 1 "Kyoto")
"Kyoto"
(aset place 3 (make-vector 5 nil))
[nil nil nil nil nil]
(aset place 2 (make-vector 5 nil))
[nil nil nil nil nil]
(aset place 4 (make-vector 5 nil))
[nil nil nil nil nil]
(aref place 1)
"Kyoto"
(aref place 2)
[nil nil nil nil nil]
(aref (aref place 2) 2)
nil
(aset (aref place 2) 2 "neko")
"neko"
(aref (aref place 2) 2)
"neko"

;; 修正時刻: Thu Mar  4 15:53:51 2021
