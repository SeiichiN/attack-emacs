;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(date-to-date)
(current-time-string)
"Sat Mar  6 05:07:34 2021"

(defvar goblin '((name . "ゴブリン") (attack-point . 20) (life-point . 60)))
(defvar oak '((name . "オーク") (attack-point . 50) (life-point . 80)))
(defvar dragon '((name . "ドラゴン") (attack-point . 80) (life-point . 100)))
(defvar alist-monster '((0 . goblin) (1 . oak) (2 . dragon)))
alist-monster


(defvar alist-monster '((0 . '((name . "ゴブリン") (attack-point . 20) (life-point . 60)))
                        (1 . '((name . "オーク") (attack-point . 50) (life-point . 80)))
                        (2 . '((name . "ドラゴン") (attack-point . 80) (life-point . 100)))))
alist-monster

alist-monster


(defun select-monster ()
  (cdr (assoc (random 3) alist-monster)))
select-monster
(select-monster)
dragon
(listp (select-monster))
nil
(listp alist-monster)
t
(listp (cdr (assoc 0 alist-monster)))
nil

nil
(listp dragon)
t

select-monster

select-monster

select-monster

select-monster
(select-monster)
(listp goblin)
t
(select-monster)
(listp (select-monster))

goblin
(cdr (assoc 'name (select-monster)))
(select-monster)

dragon




"ゴブリン"



(defvar monster)
monster
(defvar monster (select-monster))
monster

goblin
monster
goblin

(cdr (assoc 'name (cdr (assoc 0 alist-monster))))



monster
goblin
(cdr (assoc 'name dragon))
"ドラゴン"



dragon

goblin

goblin

oak




(defconst goblin '((name . "ゴブリン") (attack-point . 20) (life-point . 60)))
goblin
(get-monster-name goblin)
"ゴブリン"
(cdr (assoc 'attack-point goblin))
20

goblin

(defvar oak '((name . "オーク") (attack-point . 50) (life-point . 80)))
oak

(defvar dragon '((name . "ドラゴン") (attack-point . 80) (life-point . 100)))
dragon


(defun get-monster-name (mon)
  (interactive)
  (cdr (assoc 'name mon)))
get-monster-name

(get-monster-name dragon)
"ドラゴン"

"Oak"

"Oak"

"goblin"



oak

goblin

goblin
(setq goblin '((name . "goblin") (attack-point . 20) (life-point . 80)))
((name . "goblin") (attack-point quote goblin-at-point) (life-point . 80))

((name . "goblin") (attack-point . goblin-at-point) (life-point . 80))

((name . "goblin") (attack-point . 20) (life-point . 80))

((name . "goblin") (attack-point random 20) (life-point . 80))
(message (cdr (assoc 'name goblin)))
"goblin"

"goblin"

goblinnil

"goblin"

(quote goblin-at-point)

goblin-at-point

7
(defun goblin-at-point () random 20)
goblin-at-point

20

(random 20)
(random 20)
6



;; 修正時刻: Sat Mar  6 08:14:28 2021
