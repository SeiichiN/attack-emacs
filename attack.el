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
(defconst goblin '((name . "ゴブリン") (attack-point . 20) (life-point . 60)))
(defconst oak    '((name . "オーク")   (attack-point . 50) (life-point . 80)))
(defconst dragon '((name . "ドラゴン") (attack-point . 80) (life-point . 100)))
(defconst alist-monster '((0 . "goblin") (1 . "oak") (2 . "dragon")))

(defconst at-monster-menu "a: 戦う   b: 逃げる > ")
(defconst direction-choice "e:東 n:北 w:西 s:南 (q:終了) > ")
(defconst edge-max 10)                  ; 1辺のマス数。

(defconst attack-power '((0 . 10) (1 . 20) (2 . 30) (3 . 40) (4 . 50)))


(defun get-place (y x)
  "(Y X)位置のセルの内容を取得する."
  (aref (aref place y) x))

(defun set-place (y x thing)
  "(Y X)位置のセルに THING をセットする."
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

(defun get-monster-name (mon)
  (interactive)
  (cdr (assoc 'name mon)))

(defun select-monster ()
  (cdr (assoc (random 3) alist-monster)))

(defun monster-attack ()
  (interactive)
  (let ((mons (select-monster))
        mons-name
        mons-attack-p
        mons-life-p)
    ;; (let ((mons "oak"))
    (cond
     ((equal mons "goblin")
      (progn
        (setq mons-name (cdr (assoc 'name goblin)))
        (setq mons-attack-p (cdr (assoc 'attack-point goblin)))
        (setq mons-life-p (cdr (assoc 'life-point goblin)))))
     ((equal mons "oak")
      (progn
        (setq mons-name (cdr (assoc 'name oak)))
        (setq mons-attack-p (cdr (assoc 'attack-point oak)))
        (setq mons-life-p (cdr (assoc 'life-point oak)))))
     ((equal mons "dragon")
      (progn
        (setq mons-name (cdr (assoc 'name dragon)))
        (setq mons-attack-p (cdr (assoc 'attack-point dragon)))
        (setq mons-life-p (cdr (assoc 'life-point dragon)))))
     t)
    (attack mons-name mons-attack-p mons-life-p)))


(defun set-monster ()
  "モンスターをランダムに配置する."
  (let ((count 0))
    (setq monster-list ())
    (while (< count edge-max)
      (let ((y (random edge-max))              ; ランダムに y を決定
            (x (random edge-max)))             ; ランダムに x を決定
        (if (equal (get-place y x) nil)        ; (y x) が nil ならば
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
  (if (equal nowpos "monster")
      (monster-attack))                      ; 現在位置にモンスターがいれば
  (while (equal game-status "play")          ; "play"の間は続行
    (move)                                   ; プレイヤーの移動
    (print-nowpos)                           ; 移動した位置の情報を表示
    (if (equal nowpos "monster") (monster-attack)))  ; もしモンスターがいたら
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


(defun attack (monster-name monster-attack-p monster-life-p)
  "モンスターを攻撃する."
  (interactive)
  (let ((monster-lp monster-life-p)    ; monster-lp -- モンスターのライフポイント
        (hero-lp 100)                  ; hero-lp -- 勇者のライフポイント
        (hero-attack-p 30)
        (attack-end nil)
        monster-damage
        hero-damage
        choice)          ; choice -- 戦うか逃げるか
    (insert "モンスターが現れた。勇者はどうする？\n")
    (setq choice (read-string at-monster-menu))
    ;; a -- 攻撃。モンスターもプレーヤーもライフポイントが0以上。
    (while (and (equal choice "a") (> monster-lp 0) (> hero-lp 0))
      (setq monster-lp (attack-monster "勇者" monster-name hero-attack-p monster-lp))
      (if (> monster-lp 0)
          (setq hero-lp (attack-hero monster-name "勇者" monster-attack-p hero-lp)))
      (if (< monster-lp 1)
          (progn
            (insert "勇者はモンスターを倒した\n")
            (win-at-monster)
            (print-nowpos)
            (setq attack-end t)))
      (if (< hero-lp 1)
          (progn
            (insert "勇者はモンスターにやられてしまった\n")
            (setq game-status "end")
            (setq attack-end t)))
      (if (equal attack-end nil)
          (setq choice (read-string at-monster-menu))))
    (if (equal attack-end nil)
        (insert "勇者は逃げた。ひたすら逃げた。\n"))))

(defun win-at-monster ()
  "モンスターをやっつけたら、その位置からモンスターを消去."
  (set-place nowpos-y nowpos-x nil))

;; @param
;;   monster-name -- String "goblin" / "oak" / "dragon"
;;   hero  -- String "勇者"
;;   monster-attack-point -- int モンスターの攻撃力
;;   hero-life-point -- int heroのライフポイント
;; @return
;;   hero-life-point -- int ダメージを減算したあとのライフポイント
;;
(defun attack-hero (monster-name hero monster-attack-point hero-life-point)
  "MONSTER-NAME が HERO を攻撃して HERO の LIFE-POINT を減らす."
  (let ((damage (random monster-attack-point)))
    (setq hero-life-point (- hero-life-point damage))
    (insert (format "%s の攻撃!\n" monster-name))
    (insert (format "%s は %d のダメージを負った。hpは %d になった。\n"
                    hero damage hero-life-point))
    hero-life-point))

;; @param
;;   hero  -- String "勇者"
;;   monster-name -- String "goblin" / "oak" / "dragon"
;;   hero-attack-point -- int heroの攻撃力
;;   monster-life-point -- int monsterのライフポイント
;; @return
;;   monster-life-point -- int ダメージを減算したあとのライフポイント
;;
(defun attack-monster (hero monster-name hero-attack-point monster-life-point)
  "HERO が MONSTER を攻撃して MONSTER の LIFE-POINT を減らす."
  (let ((damage (random hero-attack-point)))
    (setq monster-life-point (- monster-life-point damage))
    (insert (format "%s の攻撃!\n" hero))
    (insert (format "%s は %d のダメージを負った。hpは %d になった。\n"
                    monster-name damage monster-life-point))
    monster-life-point))


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
  (insert ";;-------------------- ")
  (insert (current-time-string))
  (insert " ----\n")
  (while monster-list
    (insert (format ";; y:%d x:%d <- monster\n"
                    (car (car monster-list))
                    (car (cdr (car monster-list)))))
    (setq monster-list (cdr monster-list))))

;; モンスター情報をここに出力する.
;; monster-info-area
;;-------------------- Sat Mar  6 22:02:16 2021 ----
;; y:2 x:5 <- monster
;; y:4 x:8 <- monster
;; y:9 x:6 <- monster
;; y:0 x:0 <- monster
;; y:7 x:1 <- monster
;; y:7 x:6 <- monster
;; y:4 x:6 <- monster
;; y:8 x:1 <- monster
;; y:5 x:6 <- monster
;; y:6 x:9 <- monster






;; 修正時刻: Sat Mar  6 22:04:43 2021

(provide 'attack)
;;; attack.el ends here
