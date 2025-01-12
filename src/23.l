A huszonharmadik feladat gráfokról szól, a teljes részgráfokat kell keresnünk.
Első feladatunk az input feldolgozása, minden sort - ami egy-egy él tartalmaz -
két részre kell bontani, és úgy eltárolni.

(de read-input (File)
   (in File
      (make
      (until (eof)
         (link (mapcar pack (split (line) "-")))))))

Ezután tekintünk két globális változót, mely kezdetben még üres.

(setq *Neighbours NIL)
(setq *Triplets NIL)


# Irányítatlan gráfról van szó, a könnyebb feldolgozás érdekében az éllistából
# szomszédsági listát generálunk - listák listáját -, ahol az allisték feje
# a csúcs, a farok pedig a szomszédokat tartalmazza. Ezt az alábbi függvény
# konstruálja meg, felülírva az egyik globális változónkat. Érdemes csak egyszer
# meghívni, mert másodjára már fals eredmények kapunk.

(de insert-into (Key Value)
   (let Pair (assoc Key *Neighbours)
      (if (not Pair)
         (push '*Neighbours (cons Key (list Value)))
         (del Pair '*Neighbours)
         (push '*Neighbours (cons Key (cons Value (cdr Pair)))))))

# Az első feladatban háromelemű teljes gráfokat keresünk. Ehhez az kell,
# hogy a megadott három csúcs közül bármelyik csúcs szomszédja legyen a
# másik két csúcs. A feladat még leszűkíti a megoldásokat a t betűvel kezdődő
# csúcsokra, azt is elintézzük itt.

(de triplet (SetA SetB SetC)
   (and
      (member (car SetA) (cdr SetB))
      (member (car SetA) (cdr SetC))
      (member (car SetB) (cdr SetA))
      (member (car SetB) (cdr SetC))
      (member (car SetC) (cdr SetA))
      (member (car SetC) (cdr SetB))
      (or (pre? "t" (car SetA)) (pre? "t" (car SetB)) (pre? "t" (car SetC)) )))

# Adódik az ötlet, tekintsünk minden csúcshármast, és teszteljük az előbbi
# függvénnyel, hogy teljes gráfot alkotnak, vagy sem. Mivel többezer élünk van,
# ennek megfelelően elég sok csúcsunk, inkább finomítsunk ezen egy kicsit.
# Menjünk végig minden egyes csúcson, vizsgáljuk meg annak a szomszédjait,
# illetve majd a közös szomszédokat. Ha még ábécésorrendben is vannak - hogy ne
# számoljuk ugyanazt a hármast többször is -, akkor már jöhet ez a függvény -
# bár a konstrukció miatt valószínűleg kigyomlálható belőle pár feltétel.

(de find-triplets ()
   (for SetA *Neighbours
      (let HeadA (car SetA)
         (for HeadB (cdr SetA)
            (let SetB (assoc HeadB *Neighbours)
               (for HeadC (sect SetA SetB)
                  (when (< HeadA HeadB HeadC)
                     (let SetC (assoc HeadC *Neighbours)
                        (when (triplet SetA SetB SetC)
                           (push '*Triplets (list HeadA HeadB HeadC)))))))))))

# Nincs más dolgunk, mint szépen összerakni az eddig ismertetett függbényeket.
# S mivel csak a darabszám a kérdéses, megszámolni a hármasokat.

(de part1 (File)
   (let Pairs (read-input File)
      (for P Pairs
         (insert-into (car P) (cadr P))
         (insert-into (cadr P) (car P)) )
      (find-triplets)
      (println (uniq *Triplets))
      (println (length (uniq *Triplets)))
   ))

# Part 2

# Túl gyakran nem ismételtük magunkat, de most újra használnunk kell a
# 99 PicoLisp program összeállításban szereplő kódot:

(de consecDups (Lst)
   (make
      (let Last NIL
         (for X Lst
            (if (= X (car Last))
               (conc Last (cons X))
               (link (setq Last (cons X))) ) ) ) ) )
(de encode (Lst)
   (mapcar
      '((X) (cons (car X) (length X)))
      (consecDups Lst) ) )

# A második részben a feladatunk a legnagyobb teljes részgráf megkeresése.
# Ehhez nézzük meg, hogy hogyan néz ki egy adott csúcs környezetének a
# környezete, mely csúcsokat hányszor tartalmazza. Értelemszerűen a szóban
# forgó csúcs fordul elő legtöbbször, épp ezért az a kérdés, hogy a második
# helyen mekkora holtverseny van. Ha ebbe beszámítjuk a győztes is, és a
# a csúcsok számossága megegyezik előfordulásuk számával, akkor teljes gráfot
# találtunk, és visszaadjuk ezeket a csúcsokat. (Egyébként pedig NIL-t.)

(de find-group (Node)
   (let (
      Neighbourhood (filter '((X) (member Node X)) *Neighbours)
      Multiplicity (encode (sort (make
         (for Ns Neighbourhood
            (for N Ns (link N))))))
      Order (reverse (by cdr sort Multiplicity))
      Second (cdadr Order)
      Group (filter '((X)(<= Second (cdr X))) Multiplicity))
       (if (= (length Group) Second) (mapcar car Group))))

# A feladat megoldásához megint elkészítjük a szomszédsági listát, ügyeljünk
# arra, hogy ez az első lépés legyen! Majd az előbbi függvényt minden csúcsra
# lefuttatjuk. Végül nincs más dolgunk, mint a legnagyobb (rész)listát
# kiválasztjuk az eredményekből. Ügyeljünk, hogy kell bevinni az eredményt!

(de part2 (File)
   (let Pairs (read-input File)
      (for P Pairs
         (insert-into (car P) (cadr P))
         (insert-into (cadr P) (car P)) ))
   (let (
      Nodes (mapcar car *Neighbours)
      Groups (make (for N Nodes (link (find-group N)))))
      (maxi length Groups)))

