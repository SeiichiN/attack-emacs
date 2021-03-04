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
(defvar game-status)
(defvar nowpos-x)
(defvar nowpos-y)
(defvar nowpos)
(defconst at-monster-menu "a: 戦う   b: 逃げる > ")
(defconst direction-choice "e:東 n:北 w:西 s:南 (q:終了) > ")

(defun get-place (y x)
  (aref (aref place y) x))

(defun set-place (y x thing)
  (aset (aref place y) x thing))

(defun make-area ()
  (setq place (make-vector 5 nil))
  (aset place 0 (make-vector 5 nil))
  (aset place 1 (make-vector 5 nil))
  (aset place 2 (make-vector 5 nil))
  (aset place 3 (make-vector 5 nil))
  (aset place 4 (make-vector 5 nil))
  (set-monster))
;; y:4 x:1 <- monster
;; y:0 x:2 <- monster
;; y:2 x:1 <- monster
;; y:0 x:3 <- monster
;; y:3 x:1 <- monster


(defun set-monster ()
  (let ((count 0))
    (while (< count 5)
      (let ((y (random 5))
            (x (random 5)))
        (if (equal (get-place y x) nil)
            (progn
              (set-place y x "monster")
              (insert (format ";; y:%d x:%d <- monster\n" y x))
              (setq count (+ count 1))
              )
          )))))

(defun game()
  (interactive)
  (make-area)
  (make-adventure-world)
  (setq game-status "play")
  (setq nowpos-y (random 5))
  (setq nowpos-x (random 5))
  (print-nowpos)
  (if (equal nowpos "monster") (attack))
  (while (equal game-status "play")
    (move)
    (print-nowpos)
    (if (equal nowpos "monster") (attack))
    )
  (insert "ゲーム終了"))

(defun move ()
  (let (dir)
    (setq  dir (select-direction))
    (cond
     ((equal dir "n")
      (setq nowpos-y (- nowpos-y 1))
      (if (< nowpos-y 0)
          (setq nowpos-y 0)))
     ((equal dir "s")
      (setq nowpos-y (+ nowpos-y 1))
      (if (> nowpos-y 4)
          (setq nowpos-y 4)))
     ((equal dir "w")
      (setq nowpos-x (- nowpos-x 1))
      (if (< nowpos-x 0)
          (setq nowpos-x 0)))
     ((equal dir "e")
      (setq nowpos-x (+ nowpos-x 1))
      (if (> nowpos-x 4)
          (setq nowpos-x 4)))
     ((equal dir "q")
      (setq game-status "end")))))

(defun print-nowpos ()
  (setq nowpos (get-place nowpos-y nowpos-x))
  (insert (format "現在位置 Y:%d X:%d\n" nowpos-y nowpos-x))
  (insert (format " -- %s\n" nowpos)))

(defun select-direction ()
  (interactive)
  (let (direction)
    (setq direction (read-string direction-choice))
    direction))

(defun attack ()
  (interactive)
  ;;(make-adventure-world)
  (let ((monster 100)    ; monster -- モンスターのライフポイント
        (hero 100)       ; hero -- 勇者のライフポイント
        (attack-end nil)
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
            (win-at-monster)
            (print-nowpos)
            (setq attack-end t)))
      (if (< hero 1)
          (progn
            (insert "勇者はモンスターにやられてしまった\n")
            (setq game-status "end")
            (setq attack-end t)))
      (if (equal attack-end nil)
          (setq choice (read-string at-monster-menu))))
    (if (equal attack-end nil)
        (insert "勇者は逃げた。ひたすら逃げた。"))))

(defun win-at-monster ()
  (set-place nowpos-y nowpos-x nil))

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
  (insert "===================================================\n\n")
  (insert "       0 1 2 3 4 X\n")
  (insert "   Y 0|.|.|.|.|.|             北\n")
  (insert "     1|.|.|.|.|.|             |\n")
  (insert "     2|.|.|.|.|.|         西--+--東\n")
  (insert "     3|.|.|.|.|.|             |\n")
  (insert "     4|.|.|.|.|.|             南\n\n")
)






;; 修正時刻: Thu Mar  4 22:55:54 2021

(provide 'attack)
;;; attack.el end here
