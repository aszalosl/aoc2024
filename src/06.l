
# A negyedik feladathoz hasonlóan listák listájába olvassuk be a feladat táblázatát.
(de load-puzzle (File)
   (in File
      (setq *Table
         (make
            (until (eof)
               (link (line)) )))))

# Meg kell határoznunk, hogy hol van a kezdőpozíció, milyen iránnyal kezdünk, és mekkorák a tábla méretei.
# Az egyszerűség kedvéért ezek is globális változókban kerülnek eltárolásra.
(de search-start ()
   (setq *Column (length *Table))
   (setq *Row (length (car *Table)))
   (setq *Dir 0)
   (for (Y . L) *Table
      (when
         (member "\^" L)
         (setq *Y Y)
         (setq *X (inc (- (length L) (length (member "\^" L))))) )))

# A szabályok szerint addig kell előre haladni, amíg akadályba nem ütközünk (#),
# s ott jobbra kell elfordulni. Mindez történik mindaddig amig le nem lépünk a
# tábláról.
# Szerencsére a get hamisat ad, ha inkorrekt indexekkel hívjuk meg. A cond feltételeit
# a megadott sorrendben ellenőrzi, ezért kell előbb tesztelni, hogy valaminek
# nekiment - és ekkor fordul -, mint hogy halad előre.
(de move ()
   (until (or (< *X 1) (< *Row *X) (< *Y 1) (< *Column *Y))
      (put *Table *Y *X "X")
      (cond
         #up
         ((and (= *Dir 0) (= (get *Table (dec *Y) *X) "#")) (setq *Dir 1))
         ((= *Dir 0) (dec '*Y))
         #right
         ((and (= *Dir 1) (= (get *Table *Y (inc *X)) "#")) (setq *Dir 2))
         ((= *Dir 1) (inc '*X))
         #down
         ((and (= *Dir 2) (= (get *Table (inc *Y) *X) "#")) (setq *Dir 3))
         ((= *Dir 2) (inc '*Y))
         #left
         ((and (= *Dir 3) (= (get *Table *Y (dec *X)) "#")) (setq *Dir 0))
         ((= *Dir 3) (dec '*X))
         (T (prinl "Why?")) )))

# Számoljuk össze, hogy hány meglátogatott mező van a tábla egy sorában,
(de count-row (L)
   (length (filter '((C) (= "X" C)) L)) )\

# illetve az egész táblán.
(de count-steps ()
   (apply + (mapcar count-row *Table)))

(de part1 (File)
   (load-puzzle File)
   (search-start)
   (move)
   (prinl (count-steps)))

# A második részben már végtelen ciklusokat kell keresnünk, ezért módosítani kell a mozgás rutinján.
# Míg előbb akkor ért véget a vizsgálódás, ha leléptünk a tábláról, most akkor is abbahagyhatjuk,
# ha újra ugyanott, ugyanúgy haladunk. Első elképzelésem az volt, hogy a kiinduló pozíciót kell figyelni,
# de a második mintapélda ezt egyből megcáfolja.
# Az egész utat a fordulópontok határozzák meg, így elegendő azokat eltárolni, s ha ismétlődést találunk,
# akkor biztos, hogy ciklusba futottunk. Persze vigyázni kell arra, ha egy sarokba fut be az őr, akkor
# duplán fordul, de ez nem minősül ismétlődésnek.

(de move2 ()
   (let (Turns NIL Cycle NIL)
      (until (or (< *X 1) (< *Row *X) (< *Y 1) (< *Column *Y) Cycle)
         (cond
            (
               (and (= *Dir 0) (= (get *Table (dec *Y) *X) "#"))
               (setq *Dir 1)
               (ifn (member (list *X  *Y) (cdr Turns))
                  (setq Turns (cons (list *X *Y) Turns))
                  (setq Cycle T) ))
            ((= *Dir 0) (dec '*Y))
            (
               (and (= *Dir 1) (= (get *Table *Y (inc *X)) "#"))
               (setq *Dir 2)
               (ifn (member (list *X  *Y) (cdr Turns))
                  (setq Turns (cons (list *X *Y) Turns))
                  (setq Cycle T) ))
            ((= *Dir 1) (inc '*X))
            (
               (and (= *Dir 2) (= (get *Table (inc *Y) *X) "#"))
               (setq *Dir 3)
               (ifn (member (list *X  *Y) (cdr Turns))
                  (setq Turns (cons (list *X *Y) Turns))
                  (setq Cycle T) ))
            ((= *Dir 2) (inc '*Y))
            (
               (and (= *Dir 3) (= (get *Table *Y (dec *X)) "#"))
               (setq *Dir 0)
               (ifn (member (list *X  *Y) (cdr Turns))
                  (setq Turns (cons (list *X *Y) Turns))
                  (setq Cycle T) ))
            ((= *Dir 3) (dec '*X))
            (T (quit))) )
      Cycle ))


(de part2 (File)
   (load-puzzle File)
   (search-start)
   (setq *StartX *X *StartY *Y)
   (move) # calculate the original route
   (let Counter 0
      (for X *Row
         (for Y *Column
            (when (= (get *Table Y X) "X")
               (put *Table Y X "#") # check an obstacle
               (setq *X *StartX *Y *StartY *Dir 0)
               (when (move2) (inc 'Counter))
               (put *Table Y X "X") ))) # remove the obstacle
      (prinl Counter) ))

# Csupán a teszteléshez
(de print-table ()#
   (for X *Table (prinl (pack X)))
   (prinl) )

