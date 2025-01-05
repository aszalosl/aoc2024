# A 13. feladat egy kis lineáris algebrát tartalmaz. Kétismeretlenes egyenleteket
# kell megoldani. Mivel a Picolisp nem igazán szereti a lebegőpontos számokat,
# azért berakunk egy kis ellenőrzést a számolásba.

(de calc (X1 X2 Y1 Y2 Xb Yb)
   (let (
      D (- (* X1 Y2) (* Y1 X2))
      U (- (* Y2 Xb) (* Y1 Yb))
      V (- (* X1 Yb) (* X2 Xb))
      A (*/ 1 U D)
      B (*/ 1 V D)
      # testing
      XbT (+ (* A X1) (* B Y1))
      YbT (+ (* A X2) (* B Y2)))

      (if (and (= Xb XbT) (= Yb YbT)) (+ (* 3 A) B) 0) ))

# A megoldás legbonyolultabb rész az input feldolgozása. A sorokat már
# szétcincáltuk karakterekre. A karakterlistákat adott karakter mentén
# szét lehet tördelni, ezt használjuk mindkét esetben. Egy kicsit trükközünk
# előtte, hogy rövidebb legyen a rutin.
# Mivel három külön sorban szerepel a várt hat adat, egy sorként kezelt
# listában gyűjtjük azokat a feldolgozás előtt.

(de load-input (File Fn)
   (in File
      (let (Queue NIL
           Coins (make
         (until (eof)
            (let Line (line)
               (when (= "B" (car Line))
                  (let (
                     RLine (replace Line "," "+")
                     Pieces (split RLine "+")
                     A (format (pack (cadr Pieces)))
                     B (format (pack (cadddr Pieces))))
                     (queue 'Queue A)
                     (queue 'Queue B) ))
               (when (= "P" (car Line))
                  (let (
                     RLine (replace Line "," "=")
                     Pieces (split RLine "=")
                     A (format (pack (cadr Pieces)))
                     B (format (pack (cadddr Pieces))))
                     (link
                        (apply Fn
                           (list
                              (pop 'Queue)
                              (pop 'Queue)
                              (pop 'Queue)
                              (pop 'Queue)
                              A
                              B))) ))))))
         (apply + Coins) )))

# Az előbbi rutinnak paramétere volt, hogy melyik függvényt használjuk,
# ezért ezt kell megadni, illetve a fájl nevét.

(de part1 (File)
   (load-input File 'calc))

# Part 2
# Apróbb korrekció van a számolásban, amit könnyű implementálni.
(de calc2 (X1 X2 Y1 Y2 Xb Yb)
   (calc X1 X2 Y1 Y2 (+ Xb 10000000000000) (+ Yb 10000000000000)))

# Viszont így csak a függvényt kell lecserélni, és kész is vagyunk.
(de part2 (File)
   (load-input File 'calc2))

