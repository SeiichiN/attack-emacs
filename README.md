# attack-emacs
Emacs-Lispの練習

ひさしぶりに Emacs-Lisp をさわってみた。
だいぶ忘れている。

勉強用においておく。


## 使いかた

C-x C-f で attack.el を読み込む。
M-x eval-buffer で、attack.el を評価する。

M-x game とすると、新しいバッファが開いてゲームが開始する。

## 遊び方

@ がこのゲームの主人公「勇者」である。

移動は、n:北(上) e:東(右) s:南(下) w:西(左) である。

i で、所持品のリストを見ることができる。

q で、終了できる。

## モンスターとの戦い

モンスターは全部で10頭。ゴブリン、オーク、ドラゴンである。

ゴブリンは一番弱い。おそらく簡単に勝てる。

オークは手強い。オークの攻撃力がすごい。

ドラゴンは、強過ぎ(笑)。

それぞれ、a キーで攻撃。b キーで逃げることができる。

逃げるのは簡単にできる。

勇気を出して、もう一度そのセルに戻ってみると、別のモンスターになっているかもしれない。どのモンスターが出てくるかは決まっていないのである。

モンスターに勝つと gold がもらえる。いくらもらえるかは決まっていない。

## ゴールド

あちらこちらに gold が置いてある。

## ゲームの終了

まだ、未完成なので、終了はない(笑)



<!-- 修正時刻: Tue Mar  9 14:19:21 2021 -->
