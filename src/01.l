# Első lépésben be kellene olvasni az inputot, amely két oszlopot tartalmaz.
# A read segítségével beolvashatunk egy számot, ezt megismételjük, és a két számból
# készítünk egy listát. Ez minden sorra megismételve listák listáját alakíthatunk ki
# a make-link párossal. Majd végül a mapcar - mely máshol csak a map névre hallgat -
# útján le tudjuk választani a belső listák első, illetve második tagját választani.
# Ezzel az input két oszlopa két listában lesz elérhető.
# Mivel a feladat rendezett inputtal dolgozik, egyből sorba is rendezzük a listákat.

(de input (File)
   (in File
	  (let Input (make (until (eof) (link (list (read) (read)))))
         (setq *First (sort (mapcar car Input)))
         (setq *Second (sort (mapcar cadr Input)))  ) ) )

# Az első részben a listák egymásnak megfelelő tagjai eltérését kell számolni,
# majd ezeket a számokat összegezni. Itt remekül használhatjuk azt, hogy a mapcar
# nem csak egy listával képes dolgozni, hanem többel is.
(de part1 ()
    (let Result1 (apply + (mapcar '((X Y) (abs (- X Y))) *First *Second))
       (prinl Result1)))

# Part 2 ---------------------------------
# Az egymást követő azonos tagok összevonása rég megoldott [probléma](https://picolisp.com/wiki/?99p09),
ezért ezt csak ide másoljuk:

(de consecDups (Lst)
   (make
      (let Last NIL
         (for X Lst
            (if (= X (car Last))
               (conc Last (cons X))
               (link (setq Last (cons X))))))))


# Viszont ennyi még nem elég a boldogsághoz, nekünk nem a részlisták kellenek, hanem azok hossza.
# Ezért végigmegyünk a külső lista elemein, és kételemű listákat hozunk létre az eredeti elemmel,
# illetve annak multiplicitásával.

(de counter (Lst)
   (mapcar
      '((X) (list (car X) (length X)))
      (consecDups Lst) ) )

# A feladat szabályai szerint a keresett számot a multiplicitással kell beszorozni, vagy ha nem szerepel, akkor 0 az eredmény.
(de look (X)
   (let Occurs (assoc X *SecondCounter)
      (if Occurs
         (* X (cadr Occurs))
         0)))

# Nincs más dolgunk, mint végigmenni az első listán, s az elemeire alkalmazni az előbbi függvényt. A kapott számokat pedig összegezni kell.
(de part2 ()
   (setq *SecondCounter (counter *Second))
   (let Result2 (apply + (mapcar look *First))
      (prinl Result2)))
