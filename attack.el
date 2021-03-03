;;; attack.el
;;;
;;; p.107あたりのコードを elisp でやってみた


(defun attack ()
  (interactive)
  (let ((monster 100)    ; monster -- モンスターのライフポイント
        (hero 100)       ; hero -- 勇者のライフポイント
        monster-damage
        hero-damage)
    (insert "モンスターが現れた。勇者は戦いを挑んだ。\n")
    (insert "戦闘スタート!\n")
    (while (and (> monster 0) (> hero 0))
      (setq monster (attack-enemy "勇者" "モンスター" monster))
      (if (> monster 0)
          (setq hero (attack-enemy "モンスター" "勇者" hero)))
      (if (< monster 1)
          (insert "勇者はモンスターを倒した\n"))
      (if (< hero 1)
          (insert "勇者はモンスターにやられてしまった\n")))))

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

;; 修正時刻: Wed Mar  3 21:52:43 2021
