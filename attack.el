;;; package -- Summery
;;; Commentary:
;;; attack.el
;;;
;;; ==================================================================
;;; 遊びかた
;;; "attack-emacs"ディレクトリを ".emacs.d" ディレクトリの中に置く。
;;; "init.el" に、以下の記述を加える。
;;; ----------------------------------------------------
;;; (add-to-list 'load-path "~/.emacs.d/attack-emacs")
;;; (load "attack">
;;; (global-set-key "\C-cg" 'game)
;;; ----------------------------------------------------
;;; あとは、C-c g でゲームが始まる。
;;; ==================================================================
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
(defvar inventory-list)                 ; プレイヤーの持ち物
;; goldをゲットしたら t にする。そして、"goldをゲットした" と表示したら、すぐに
;; 消すために nil にもどす
(defvar show-short-message nil)

(defvar monster-list)
(defconst goblin '((name . "ゴブリン") (attack-point . 20) (life-point . 60) (gold . 10)))
(defconst oak    '((name . "オーク")   (attack-point . 50) (life-point . 80) (gold . 80)))
(defconst dragon '((name . "ドラゴン") (attack-point . 80) (life-point . 100) (gold . 200)))
(defconst alist-monster '((0 . "goblin") (1 . "oak") (2 . "dragon")))

(defconst at-monster-menu "a: 戦う   b: 逃げる > ")
(defconst direction-choice "e:東 n:北 w:西 s:南 (i:持ち物を見る q:終了) > ")
(defconst edge-max 10)                  ; 1辺のマス数。

(defconst attack-power '((0 . 10) (1 . 20) (2 . 30) (3 . 40) (4 . 50)))
(defconst message-area-line 22)

(defvar gold-list '())
(defconst gold-min 20)
(defconst gold-max 80)                  ; 20...100 にするつもり.
(defconst logfile "~/.emacs.d/attack-emacs/attack.log")

(defconst hero-life-point 100)          ; default 100
(defconst hero-attack-point 30)        ; default 30

(defun get-place (y x)
  "(Y X)位置のセルの内容を取得する."
  (aref (aref place y) x))

(defun set-place (y x thing)
  "(Y X)位置のセルに THING をセットする."
  (aset (aref place y) x thing))

(defun count-list (lst)
  "リスト(LST)の数を返す."
  (defun count-list-in (lst n)
    (if (null (car lst))
        n
      (count-list-in (cdr lst) (+ n 1))))
  (count-list-in lst 0))


(defun set-inventory ()
  "INVENTORY-LIST を初期化する."
  (setq inventory-list '((gold . 0) (weapon . "non"))))

(defun make-area ()
  "ゲームエリアを作成 -- place を edge-max個の配列とし、初期値に nil をセットする."
  (setq place (make-vector edge-max nil))
  (let ((edge 0))                       ; edge -- ローカル変数
    (while (< edge edge-max)
      ;; edge番目の place に edge-max個の配列を作成し、初期値に nil をセット。
      (aset place edge (make-vector edge-max nil))
      (setq edge (+ edge 1))))
  (set-inventory)
  (set-monster)                        ; monster を配置する
  (set-gold))                          ; gold を配置する

;; 5x5の2次元配列を作る。初期値は nil
;; (aset place 0 (make-vector 5 nil))
;; (aset place 1 (make-vector 5 nil))
;; (aset place 2 (make-vector 5 nil))
;; (aset place 3 (make-vector 5 nil))
;; (aset place 4 (make-vector 5 nil))


;; (defun set-power ()
;;     (cdr (assoc (random 5) attack-power)))

;; (defun get-monster-name (mon)
;;   (interactive)
;;   (cdr (assoc 'name mon)))

(defun select-monster ()
  "モンスターを決める."
  (cdr (assoc (random 3) alist-monster)))

(defun decide-monster ()
  "ここで始めて monster の種類を決める。-- select-monster \n それから、モンスターの攻撃力、耐久力を決める."
  (interactive)
  (let ((mons (select-monster))
        mons-name
        mons-attack-p
        mons-life-p)
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
    (attack mons mons-name mons-attack-p mons-life-p)))


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


(defun delete-monster-from-list (y x mons-list)
  "Y X の地点のモンスターを MONS-LIST から削除する."
  (interactive "nY: \nnX: ")
  (cond
   ((null mons-list) nil)
   ((equal (list y x) (car mons-list))
    (delete-monster-from-list y x (cdr mons-list)))
   (t (cons (car mons-list) (delete-monster-from-list y x (cdr mons-list))))))


(defun set-gold ()
  "ゴールドをランダムに配置する."
  (interactive)
  (let ((count 0))
    (setq gold-list '())
    (while (< count 5)
      (let ((y (random edge-max))
            (x (random edge-max)))
        (if (equal (get-place y x) nil)
            (progn
              (set-place y x "gold")
              (setq gold-list (cons (list y x) gold-list))
              (setq count (+ count 1)))))))
  (disp-gold-info))

(defun get-item (item val)
  "アイテムを取得したら、それを持物リストに加える.
ITEM -- アイテムの種類   VAL -- その値・名前."
  (interactive "sItem: \nnVal:")
  (cond
   ((equal item "gold")
    (let (newval)
      (setq newval (+ (cdr (assoc 'gold inventory-list)) val))
      (push (cons 'gold newval) inventory-list)))
   t)
  ;; そのセルを空にする.
  (set-place nowpos-y nowpos-x nil))

(defun game-pre ()
  "ゲームエリアの作成だけをする."
  (interactive)
  (make-area))

(defun game-end ()
  "ゲーム終了."
  (let (str)
    (setq str (concat "----------------------\n"
                      "ゲーム終了\n"
                      "獲得gold:%d\n"
                      "残りモンスター:%d\n"
                      "----------------------\n"))
    (message-area-insert
     (format str
             (cdr (assoc 'gold inventory-list))
             (count-list monster-list)))))


;; ゲーム開始時には、どこに monster がいるかはわかっているが、
;; ゴブリンなのかオークなのかドラゴンなのかは、決まっていない。
(defun game ()
  "ゲーム開始コマンド."
  (interactive)
  (make-area)                                ; ゲームエリア作成
  (make-adventure-world)                     ; タイトル画面表示
  (setq game-status "play")                  ; "play" -- ゲーム続行
  (setq nowpos-y (random edge-max))          ; 開始時の y位置
  (setq nowpos-x (random edge-max))          ; 開始時の x位置
  (move-cursor-first-and-print nowpos-y nowpos-x)
  (getinfo-nowpos)                             ; 開始時の位置の情報
  (if (equal nowpos "monster")               ; ゲーム開始いきなり...
      (decide-monster))                      ; ...現在位置にモンスターがいれば
  (while (equal game-status "play")          ; "play"の間は続行
    (if (equal game-status "end") (game-end))
    (move)                                   ; プレイヤーの移動
    (getinfo-nowpos)                           ; 移動した位置の情報を表示
    (move-cursor nowpos-y nowpos-x)
    (print-cursor)
    (if (equal nowpos "monster") (decide-monster))  ; もしモンスターがいたら
    (if (equal nowpos "gold")                ; gold だったら
        (progn
          (let (money)
            (setq show-short-message t)
            ;; 0...gold-max --> gold-min...gold-max + gold-min
            (setq money (+ (random gold-max) gold-min))
            (get-item "gold" money)
            (clean-message-area)
            (message-area-insert (format "%d の gold を手に入れた\n" money))))))
  (if (equal game-status "end")
      (game-end)))

(defun move ()
  "プレイヤーの移動およびコマンドの入力."
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
     ((equal dir "i")
      (show-inventory))
     ((equal dir "q")                        ; 'q'が押されたら
      (setq game-status "end"))))           ; game-status を "end" にする
  ;; show-short-message が t なら clean-message-area を実行する
  (if (equal show-short-message t)
      (progn
        (clean-message-area)
        (setq show-short-message nil))))

(defun move-cursor-first-and-print (y-pos x-pos)
  "ゲーム開始l Y-POS X-POS にカーソルを移動."
  (move-to-window-line (+ y-pos 9))
  (move-to-column (+ 7 (* x-pos 2)))
  (insert-char ?@ 1)
  (delete-char 1))


(defun move-cursor (y-pos x-pos)
  "現在地点を . にして Y-POS X-POS にカーソルを移動."
  (let ((current-p (point))
        new-point)
    (goto-char (- current-p 1))
    (insert-char ?. 1)
    (delete-char 1)
    (move-to-window-line (+ y-pos 9))
    (move-to-column (+ 7 (* x-pos 2)))))

(defun show-inventory ()
  "勇者の持ち物の一覧を見る."
  (clean-message-area)
  (message-area-insert (format "持ち物 gold:%d  weapon:%s  残りモンスター:%d\n"
                               (cdr (assoc 'gold inventory-list))
                               (cdr (assoc 'weapon inventory-list))
                               (count-list monster-list))))
    
(defun getinfo-nowpos ()
  "現在位置の情報を表示."
  (setq nowpos (get-place nowpos-y nowpos-x)))
  ;; (message-area-insert
  ;;  (format "現在位置 Y:%d X:%d -- %s\n"
  ;;          nowpos-y nowpos-x nowpos)))

(defun print-cursor ()
  "カーソルを表示する."
  (insert-char ?@ 1)
  (delete-char 1))

(defun message-area-insert (str)
  (interactive "sInput: String > ")
  (let ((current-p (point)))
    (move-to-window-line message-area-line)
    (move-to-column 0)
    (insert str)
    (move-to-column 0)
    (insert "\n")
    (goto-char current-p)))

;; (defun message-area-clean ()
;;   (let ((current-p (point)))
;;     (move-to-window-line message-area-line)
;;     (move-to-column 0)
;;     (kill-line 1)
;;     (goto-char current-p)))

(defun select-direction ()
  "移動方向を選択する."
  (interactive)
  (let (direction)
    (setq direction (read-string direction-choice))
    direction))


(defun attack (mons monster-name monster-attack-p monster-life-p)
  "モンスターを攻撃する.
MONS :goblin/oak/dragon
MONSTER-NAME :ゴブリン・オーク・ドラゴン
MONSTER-ATTACK-P :モンスターの攻撃力
MONSTER-LIFE-P :モンスターの耐久力"
  (interactive)
  (let ((monster-lp monster-life-p)    ; monster-lp -- モンスターのライフポイント
        (hero-lp hero-life-point)                  ; hero-lp -- 勇者のライフポイント
        (hero-attack-p hero-attack-point)
        (attack-end nil)
        monster-damage
        hero-damage
        choice)          ; choice -- 戦うか逃げるか
    (clean-message-area)
    (message-area-insert (format "%s が現れた。勇者はどうする？\n"
                                 monster-name))
    ;; (message-area-insert "...\n")
    (setq choice (read-string at-monster-menu))
    (clean-message-area)
    ;; a -- 攻撃。モンスターもプレーヤーもライフポイントが0以上。
    (while (and (equal choice "a") (> monster-lp 0) (> hero-lp 0))
      (setq monster-lp (attack-monster "勇者" monster-name hero-attack-p monster-lp))
      (if (> monster-lp 0)
          (setq hero-lp (attack-hero monster-name "勇者" monster-attack-p hero-lp)))
      (if (< monster-lp 1)
          (progn
            ;; (clean-message-area)
            (message-area-insert
             (format "勇者は %s を倒した\n" monster-name))
            (win-at-monster mons)
            ;; monster-list から倒したモンスターの (y x) を削除する
            (setq monster-list
                  (delete-monster-from-list nowpos-y nowpos-x monster-list))
            (if (null monster-list) (setq game-status "end"))
            (getinfo-nowpos)
            (setq attack-end t)))
      (if (< hero-lp 1)
          (progn
            ;; (clean-message-area)
            (message-area-insert
             (format "勇者は %s にやられてしまった\n" monster-name))
            (setq game-status "end")
            (setq attack-end t)))
      (if (equal attack-end nil)
          (progn
            (setq choice (read-string at-monster-menu))
            (clean-message-area)))
      ;; (message-area-insert "...\n")
      )
    (if (equal attack-end nil)
        (progn
          (clean-message-area)
          (message-area-insert "勇者は逃げた。ひたすら逃げた。\n")
          (setq show-short-message t)
          ;; (message-area-insert "...\n")
          ))))

(defun win-at-monster (mons-name)
  "モンスター(MONS-NAME)をやっつけたら、その位置からモンスターを消去.\nまた、ランダムな gold を入手."
  (interactive "sInput: mons-name>")
  (let (mons-gold)
    (cond
     ((equal mons-name "goblin")
      (setq mons-gold (cdr (assoc 'gold goblin))))
     ((equal mons-name "oak")
      (setq mons-gold (cdr (assoc 'gold oak))))
     ((equal mons-name "dragon")
      (setq mons-gold (cdr (assoc 'gold dragon))))
     t)
    (setq money (cdr (assoc 'gold inventory-list)))
    (setq new-money (+ money mons-gold))
    (push (cons 'gold new-money) inventory-list)
    (message-area-insert (format "goldを %d 手に入れた\n" mons-gold)))
  ;; show-short-message を t にしておいて、移動コマンドが押されたら画面消去する
  (setq show-short-message t)
  (set-place nowpos-y nowpos-x nil))

;; @param
;;   monster-name -- String "goblin" / "oak" / "dragon"
;;   hero  -- String "勇者"
;;   monster-attack-point -- int モンスターの攻撃力
;;   hero-life-p -- int heroのライフポイント
;; @return
;;   hero-life-p -- int ダメージを減算したあとのライフポイント
;;
(defun attack-hero (monster-name hero monster-attack-point hero-life-p)
  "MONSTER-NAME が HERO を攻撃して HERO の LIFE-POINT を減らす."
  (let ((damage (random monster-attack-point)))
    (setq hero-life-p (- hero-life-p damage))
    (message-area-insert
     (format "%s の攻撃! -- %s は %d のダメージを負った。%s の life-pは %d になった。\n"
             monster-name hero damage hero hero-life-p))
    hero-life-p))

;; @param
;;   hero  -- String "勇者"
;;   monster-name -- String "goblin" / "oak" / "dragon"
;;   hero-attack-p -- int heroの攻撃力
;;   monster-life-point -- int monsterのライフポイント
;; @return
;;   monster-life-point -- int ダメージを減算したあとのライフポイント
;;
(defun attack-monster (hero monster-name hero-attack-p monster-life-point)
  "HERO が MONSTER を攻撃して MONSTER の LIFE-POINT を減らす."
  (let ((damage (random hero-attack-p)))
    (setq monster-life-point (- monster-life-point damage))
    (message-area-insert
     (format "%s の攻撃! -- %s は %d のダメージを負った。%s の life-pは %d になった。\n"
             hero monster-name damage monster-name  monster-life-point))
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
  (insert "\n")
  (insert "       0 1 2 3 4 5 6 7 8 9\n")
  (insert "     0|.|.|.|.|.|.|.|.|.|.|             n北\n")
  (insert "     1|.|.|.|.|.|.|.|.|.|.|              |\n")
  (insert "     2|.|.|.|.|.|.|.|.|.|.|         w西--+--東e\n")
  (insert "     3|.|.|.|.|.|.|.|.|.|.|              |\n")
  (insert "     4|.|.|.|.|.|.|.|.|.|.|             s南\n")
  (insert "     5|.|.|.|.|.|.|.|.|.|.|\n")
  (insert "     6|.|.|.|.|.|.|.|.|.|.|\n")
  (insert "     7|.|.|.|.|.|.|.|.|.|.|\n")
  (insert "     8|.|.|.|.|.|.|.|.|.|.|\n")
  (insert "     9|.|.|.|.|.|.|.|.|.|.|\n\n")
  (insert "\n")
  (insert "\n")
  (insert "\n")
  (insert "\n")
  )

(defun clean-message-area ()
  "メッセージエリアをクリアする."
  (let ((current-p (point)))
    (goto-line (+ message-area-line 1))
    (beginning-of-line)
    (delete-region (point) (point-max))
    (goto-char current-p)))


(defun disp-info (str str-list)
  "(デバッグ用)情報をファイルに出力する。STR:情報対象、STR-LIST:情報対象のリスト."
  (save-excursion
    (set-buffer (find-file-noselect logfile))
    (disp-info-in str str-list)
    (write-file logfile)))


(defun disp-info-in (str str-list)
  "(デバッグ用) STR の情報を出力する.\n STR-LIST: strのリスト."
  (interactive)
  (goto-char (point-max))
  (search-backward (concat str "-info-area"))
  (forward-line 1)
  (insert ";;------------------- ")
  (insert (current-time-string))
  (insert " -----\n")
  (while str-list
    (insert (format ";; y:%d x:%d <- %s\n"
                    (car (car str-list))
                    (car (cdr (car str-list)))
                    str))
    (setq str-list (cdr str-list))))


(defun disp-monster-info ()
  "(デバッグ用) モンスター情報."
  (disp-info "monster" monster-list))

;; モンスター情報をここに出力する.
;; monster-info-area



(defun disp-gold-info ()
  "(デバッグ用) ゴールド情報."
  (disp-info "gold" gold-list))

;; ゴールド情報をここに出力する.
;; gold-info-area



;; 修正時刻: Sun Mar 21 21:59:10 2021

;;; (provide 'attack.el)
;;; attack.el ends here
