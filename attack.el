;;; attack.el
;;;
;;; p.107あたりのコードを elisp でやってみた


(defun attack ()
  (interactive)
  (let ((enemy 100) damage )
    (insert "モンスターが現れた。勇者は戦いを挑んだ。\n")
    (insert "戦闘スタート!\n")
    (while (> enemy 0)
      (setq damage (random 30))
      (setq enemy (- enemy damage))
      (if (< enemy 0)
          (insert "勇者はモンスターを倒した\n")
        (insert (format "モンスターに %d のダメージを与えた\n" enemy) )))))


;; 修正時刻: Wed Mar  3 16:26:10 2021
