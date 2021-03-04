;;; package -- Summery
;;; attack.el
;;; 
;;; p.107あたりのコードを elisp でやってみた

;;   0 1 2 3 4
;; 0|@|.|.|.|.|
;; 1|.|.|@|.|.|
;; 2|.|.|.|.|@|
;; 3|.|.|.|@|.|
;; 4|.|@|.|.|.|

;; @ -- モンスターの位置

(defvar place)
(defconst at-monster-menu "a: 戦う   b: 逃げる > ")
(defconst direction-choice "e:east n:north w:west s:south > ")

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

(defun game()
  (interactive)
  (let (dir)
    (make-area)
    (make-adventure-world)
    (setq  dir (select-direction))
    (cond
     ((equal dir "n")
      (message "north"))
     ((equal dir "s")
      (message "south"))
     ((equal dir "w")
      (message "west"))
     ((equal dir "e")
      (message "east"))
     )
    ))

(defun select-direction ()
  (interactive)
  (let (direction)
    (setq direction (read-string direction-choice))
    direction))

(defun attack ()
  (interactive)
  (make-adventure-world)
  (let ((monster 100)    ; monster -- モンスターのライフポイント
        (hero 100)       ; hero -- 勇者のライフポイント
        (game-end nil)
        monster-damage
        hero-damage
        choice)
    (insert "モンスターが現れた。勇者はどうする？\n")
    (setq choice (read-string at-monster-menu))
    (while (and (equal choice "a") (> monster 0) (> hero 0))
      (setq monster (attack-enemy "勇者" "モンスター" monster))
      (if (> monster 0)
          (setq hero (attack-enemy "モンスター" "勇者" hero)))
      (if (< monster 1)
          (progn
            (insert "勇者はモンスターを倒した\n")
            (setq game-end t)))
      (if (< hero 1)
          (progn
            (insert "勇者はモンスターにやられてしまった\n")
            (setq game-end t)))
      (if (equal game-end nil)
          (setq choice (read-string at-monster-menu))))
    (if (equal game-end nil)
        (insert "勇者は逃げた。ひたすら逃げた。"))))


;; @param
;;   my -- String 自分
;;   enemy -- String 攻撃対象
;;   life-point -- int 攻撃対象のライフポイント
;; @return
;;   life-point -- int ダメージを減算したあとのライフポイント
;;
(defun attack-enemy (my enemy life-point)
  (setq damage (random 30))
  (setq life-point (- life-point damage))
  (insert (format "%s の攻撃!\n" my))
  (insert (format "%s は %d のダメージを負った。hpは %d になった。\n"
                  enemy damage life-point))
  life-point)

;;
(defun make-adventure-world ()
  (interactive)
  (generate-new-buffer "adventure-world")
  (switch-to-buffer "adventure-world")
  (erase-buffer)
  (insert "===================================================\n")
  (insert "|                                                 |\n")
  (insert "|            ようこそ、冒険の世界へ!              |\n")
  (insert "|                                                 |\n")
  (insert "|               Adventure World                   |\n")
  (insert "|                                                 |\n")
  (insert "===================================================\n\n"))



;; 修正時刻: Thu Mar  4 16:27:36 2021

(provide 'attack)
;;; attack.el end here
