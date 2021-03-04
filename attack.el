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

(defvar place)                          ; ゲームエリア。配列。
(defvar game-status)                    ; "play" / "end"
(defvar nowpos-x)                       ; プレイヤーの現在位置: x位置
(defvar nowpos-y)                       ; プレイヤーの現在位置: y位置
(defvar nowpos)                         ; プレイヤーの現在位置の内容。初期値は nil。
                                        ; "monster" が格納されている場合もある。
(defconst at-monster-menu "a: 戦う   b: 逃げる > ")
(defconst direction-choice "e:東 n:北 w:西 s:南 (q:終了) > ")
(defconst edge-max 10)                  ; 1辺のマス数。

;; (y x)位置のセルの内容を取得する
(defun get-place (y x)
  (aref (aref place y) x))

;; (y x)位置のセルに thing をセットする。
(defun set-place (y x thing)
  (aset (aref place y) x thing))

;; ゲームエリアを作成する。
(defun make-area ()
  ;; place を edge-max個の配列とし、初期値に nil をセットする。
  (setq place (make-vector edge-max nil))
  (let ((edge 0))                       ; edge -- ローカル変数
    (while (< edge edge-max)
      ;; edge番目の place に edge-max個の配列を作成し、初期値に nil をセット。
      (aset place edge (make-vector edge-max nil))
      (setq edge (+ edge 1))))
  ;; monster を配置する
  (set-monster))

;; 5x5の2次元配列を作る。初期値は nil
;; (aset place 0 (make-vector 5 nil))
;; (aset place 1 (make-vector 5 nil))
;; (aset place 2 (make-vector 5 nil))
;; (aset place 3 (make-vector 5 nil))
;; (aset place 4 (make-vector 5 nil))



(defun set-monster ()
  (let ((count 0))
    (while (< count edge-max)
      (let ((y (random edge-max))
            (x (random edge-max)))
        (if (equal (get-place y x) nil)
            (progn
              (set-place y x "monster")
              (insert (format ";; y:%d x:%d <- monster\n" y x))
              (setq count (+ count 1))
              )
          )))))

(defun game-pre ()
  (interactive)
  (make-area))


(defun game ()
  (interactive)
  (make-area)
  (make-adventure-world)
  (setq game-status "play")
  (setq nowpos-y (random edge-max))
  (setq nowpos-x (random edge-max))
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
      (if (> nowpos-y (- edge-max 1))
          (setq nowpos-y (- edge-max 1))))
     ((equal dir "w")
      (setq nowpos-x (- nowpos-x 1))
      (if (< nowpos-x 0)
          (setq nowpos-x 0)))
     ((equal dir "e")
      (setq nowpos-x (+ nowpos-x 1))
      (if (> nowpos-x (- edge-max 1))
          (setq nowpos-x (- edge-max 1))))
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
  (insert "===================================================\n")
  (insert "       X\n")
  (insert "       0 1 2 3 4 5 6 7 8 9\n")
  (insert "   Y 0|.|.|.|.|.|.|.|.|.|.|             北\n")
  (insert "     1|.|.|.|.|.|.|.|.|.|.|             |\n")
  (insert "     2|.|.|.|.|.|.|.|.|.|.|         西--+--東\n")
  (insert "     3|.|.|.|.|.|.|.|.|.|.|             |\n")
  (insert "     4|.|.|.|.|.|.|.|.|.|.|             南\n")
  (insert "     5|.|.|.|.|.|.|.|.|.|.|\n")
  (insert "     6|.|.|.|.|.|.|.|.|.|.|\n")
  (insert "     7|.|.|.|.|.|.|.|.|.|.|\n")
  (insert "     8|.|.|.|.|.|.|.|.|.|.|\n")
  (insert "     9|.|.|.|.|.|.|.|.|.|.|\n\n")
)






;; 修正時刻: Fri Mar  5 07:47:30 2021

(provide 'attack)
;;; attack.el end here
