# A második feladatban azon sorokat kell összeszámolni, melyek monoton növekvőek vagy csökkenőek,
# és az egymás melletti tagok nem nagyon térnek el egymástól.
# A monotonság vizsgálatához tekintjük az egymást követő elemek differenciáit, és ezeket fogjuk vizsgálni.

(de small (X) (and (< 0 X 4)))
(de increasing (L) (apply and (mapcar small L)))
(de decreasing (L) (increasing (mapcar - L)))
(de diffs (L R)
   (if (< 1 (length L))
      (let (F (car L) S (cadr L))
         (diffs (cdr L) (cons (- S F) R)))
      R))

(de correct (L)
   (let D (diffs L '())
      (or (increasing D) (decreasing D))))

(de part1 (I)
   (let G 0
      (in I
         (until (eof)
            (let L (str (line T))
               (if (correct L)
                  (inc 'G)))))
      G))

# part 2
# A feladat második részében bármely szám kihagyható, s a maradéknak kell teljesíteni
# a korábban megfogalmazott feltételeket. Ehhez elkészítjük egy-egy sor összes variánsát,
# s ha ezek közül egy is jó (or), akkor azt a sort is megszámoljuk.
(de without (L X R C)
   (if R
         (let C1 (append L R)
            (without (append L (list X)) (car R) (cdr R) (cons C1 C)))
     (cons L C)))

(de correct2 (Ls)
   (apply or (mapcar correct Ls)))


(de part2 (I)
   (let G 0
      (in I
         (until (eof)
            (let L (str (line T))
               (let C (without '() (car L) (cdr L) '())
                  (if (correct2 C)
                     (inc 'G))))))
      G))

