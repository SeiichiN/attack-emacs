;;; package -- Summery
;;; attack.el
;;; 
;;; p.107あたりのコードを elisp でやってみた


(defun attack ()
  (interactive)
  (make-adventure-world)
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

;; 修正時刻: Thu Mar  4 07:32:51 2021

(provide 'attack)
;;; attack.el end here
