;;; package -- Summery
;;; Commentary:
;;; attack.el
;;; 
;;; p.107あたりのコードを elisp でやってみた
;;;
;;; Code:

;; 5x5の場合だと、以下のようになる。
;;
;;   0 1 2 3 4
;; 0|@|.|.|.|.|
;; 1|.|.|@|.|.|
;; 2|.|.|.|.|@|
;; 3|.|.|.|@|.|
;; 4|.|@|.|.|.|
;;
;; @ -- モンスターの位置
;; . -- nil

;; グローバル変数・定数
(defvar place)                          ; ゲームエリア。配列。
(defvar game-status)                    ; "play" / "end"
(defvar nowpos-x)                       ; プレイヤーの現在位置: x位置
(defvar nowpos-y)                       ; プレイヤーの現在位置: y位置
(defvar nowpos)                         ; プレイヤーの現在位置の内容。初期値は nil。
                                        ; "monster" が格納されている場合もある。
(defvar monster-list)
(defconst at-monster-menu "a: 戦う   b: 逃げる > ")
(defconst direction-choice "e:東 n:北 w:西 s:南 (q:終了) > ")
(defconst edge-max 10)                  ; 1辺のマス数。

(defconst attack-power '((0 . 10) (1 . 20) (2 . 30) (3 . 40) (4 . 50)))


(defun get-place (y x)
  "(y x)位置のセルの内容を取得する."
  (aref (aref place y) x))

(defun set-place (y x thing)
  "(y x)位置のセルに thing をセットする。"
  (aset (aref place y) x thing))

(defun make-area ()
  "ゲームエリアを作成 -- place を edge-max個の配列とし、初期値に nil をセットする."
  (setq place (make-vector edge-max nil))
  (let ((edge 0))                       ; edge -- ローカル変数
    (while (< edge edge-max)
      ;; edge番目の place に edge-max個の配列を作成し、初期値に nil をセット。
      (aset place edge (make-vector edge-max nil))
      (setq edge (+ edge 1))))
  (set-monster))                        ; monster を配置する

;; 5x5の2次元配列を作る。初期値は nil
;; (aset place 0 (make-vector 5 nil))
;; (aset place 1 (make-vector 5 nil))
;; (aset place 2 (make-vector 5 nil))
;; (aset place 3 (make-vector 5 nil))
;; (aset place 4 (make-vector 5 nil))


(defun set-power ()
    (cdr (assoc (random 5) attack-power)))

(defun set-monster ()
  "モンスターをランダムに配置する."
  (let ((count 0))
    (setq monster-list ())
    (while (< count edge-max)
      (let ((y (random edge-max))
            (x (random edge-max)))
        (if (equal (get-place y x) nil)
            (progn
              (set-place y x "monster")
              (setq monster-list (cons (list y x) monster-list))
              (setq count (+ count 1))
              )))))
  (disp-monster-info))        ; デバッグ用にモンスターの位置を出力する

(defun game-pre ()
  "ゲームエリアの作成だけをする."
  (interactive)
  (make-area))


(defun game ()
  "ゲーム開始コマンド."
  (interactive)
  (make-area)                                ; ゲームエリア作成
  (make-adventure-world)                     ; タイトル画面表示
  (setq game-status "play")                  ; "play" -- ゲーム続行
  (setq nowpos-y (random edge-max))          ; 開始時の y位置
  (setq nowpos-x (random edge-max))          ; 開始時の x位置
  (print-nowpos)                             ; 開始時の位置の情報
  (if (equal nowpos "monster") (attack))     ; 現在位置にモンスターがいれば
  (while (equal game-status "play")          ; "play"の間は続行
    (move)                                   ; プレイヤーの移動
    (print-nowpos)                           ; 移動した位置の情報を表示
    (if (equal nowpos "monster") (attack)))  ; もしモンスターがいたら
  (insert "ゲーム終了"))

(defun move ()
  "プレイヤーの移動."
  (let (dir)                                 ; dir -- ユーザーの入力
    (setq  dir (select-direction))
    (cond
     ((equal dir "n")                        ; n = 北
      (setq nowpos-y (- nowpos-y 1))
      (if (< nowpos-y 0)
          (setq nowpos-y 0)))
     ((equal dir "s")                        ; s = 南
      (setq nowpos-y (+ nowpos-y 1))
      (if (> nowpos-y (- edge-max 1))
          (setq nowpos-y (- edge-max 1))))
     ((equal dir "w")                        ; w = 西
      (setq nowpos-x (- nowpos-x 1))
      (if (< nowpos-x 0)
          (setq nowpos-x 0)))
     ((equal dir "e")                        ; e = 東
      (setq nowpos-x (+ nowpos-x 1))
      (if (> nowpos-x (- edge-max 1))
          (setq nowpos-x (- edge-max 1))))
     ((equal dir "q")                        ; 'q'が押されたら
      (setq game-status "end")))))           ; game-status を "end" にする

(defun print-nowpos ()
  "現在位置の情報を表示."
  (setq nowpos (get-place nowpos-y nowpos-x))
  (insert (format "現在位置 Y:%d X:%d\n" nowpos-y nowpos-x))
  (insert (format " -- %s\n" nowpos)))

(defun select-direction ()
  "移動方向を選択する."
  (interactive)
  (let (direction)
    (setq direction (read-string direction-choice))
    direction))

(defun attack ()
  "モンスターを攻撃する."
  (interactive)
  (let ((monster 100)    ; monster -- モンスターのライフポイント
        (hero 100)       ; hero -- 勇者のライフポイント
        (attack-end nil)
        monster-damage
        hero-damage
        choice)          ; choice -- 戦うか逃げるか
    (insert "モンスターが現れた。勇者はどうする？\n")
    (setq choice (read-string at-monster-menu))
    ;; a -- 攻撃。モンスターもプレーヤーもライフポイントが0以上。
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
  "モンスターをやっつけたら、その位置からモンスターを消去."
  (set-place nowpos-y nowpos-x nil))

;; @param
;;   my -- String 自分
;;   enemy -- String 攻撃対象
;;   life-point -- int 攻撃対象のライフポイント
;; @return
;;   life-point -- int ダメージを減算したあとのライフポイント
;;
(defun attack-enemy (self enemy life-point)
  "敵を攻撃するのは、モンスターも勇者も同じやり方."
  (setq damage (random 30))
  (setq life-point (- life-point damage))
  (insert (format "%s の攻撃!\n" self))
  (insert (format "%s は %d のダメージを負った。hpは %d になった。\n"
                  enemy damage life-point))
  life-point)


(defun make-adventure-world ()
  "タイトル画面を表示する."
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

(defun disp-monster-info ()
  "(デバッグ用) モンスター情報."
  (interactive)
  (goto-char (point-max))
  (search-backward "monster-info-area")
  (forward-line 1)
  (insert ";;------------------------------\n")
  (while monster-list
    (insert (format ";; y:%d x:%d <- monster\n"
                    (car (car monster-list))
                    (car (cdr (car monster-list)))))
    (setq monster-list (cdr monster-list))))

;; モンスター情報をここに出力する.
;; monster-info-area
;;------------------------------
;; y:2 x:4 <- monster
;; y:3 x:8 <- monster
;; y:7 x:6 <- monster
;; y:2 x:2 <- monster
;; y:9 x:3 <- monster
;; y:2 x:9 <- monster
;; y:5 x:2 <- monster
;; y:1 x:4 <- monster
;; y:0 x:0 <- monster
;; y:9 x:7 <- monster






;; 修正時刻: Fri Mar  5 20:20:48 2021

(provide 'attack)
;;; attack.el end here
