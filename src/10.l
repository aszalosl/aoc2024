# A tizedik feladatban utakat kell keresni. A térkép egy mátrixnak felel meg.
# ezért a korábban már szereplő, listák listáját generáló rutint kell használnunk.
# Egyetlen csavar az egészben, hogy a karakterekből számokat készítünk.

(de read-input (File)
   (make
      (in File
         (until (eof)
            (link (mapcar format (line))) ))))

# A feladat első részében az számít, hogy honnan hova jutottunk el. Épp ezért sallangként
# cipeljük magunkkal az S kezdőpozíciót. Mivel korlátozott az út hossza, sima rekurziót
# használunk. Ha lelépünk a tábláról, vagy nincs meg a kivánt növekmény, akkor az zsátutca,
# nem vesztegetünk rá kódot. Ha viszont legális a lépés, akkor ha elértünk a végpozíciót,
# akkor a globális változóba felvesszük ezt az utat is (a két végpontjával). Egyébként
# a négy égtáj irányába teszünk egy újabb lépést.

(de dfs (X Y N S)
   (when
      (and
         (<= 1 Y (length *Table))
         (<= 1 X (length (car *Table)))
         (= N (get *Table Y X)))
      (if (= N 9)
         (setq *Paths (cons (cons X Y S) *Paths))
         (dfs (dec X) Y (inc N) S)
         (dfs (inc X) Y (inc N) S)
         (dfs X (dec Y) (inc N) S)
         (dfs X (inc Y) (inc N) S) )))

# Az utak kereséséhez végighaladunk a térkép összes mezőjén, s ha ott 0 szerepel,
# akkor onnan meghívható az előbbi rekurzív függvény. Annak érdekében, hogy ne ismételjünk
# megoldásokat feleslegesen, az első lépést már itt megtesszük.

(de search (File)
   (setq *Table (read-input File) *Paths NIL)
   (for (Y . Row) *Table
      (for (X . N) Row
         (when (=0 N) # we start from 0
            (dfs (dec X) Y 1 (list X Y))
            (dfs (inc X) Y 1 (list X Y))
            (dfs X (dec Y) 1 (list X Y))
            (dfs X (inc Y) 1 (list X Y))
            ))))

# A megoldáshoz nincs másra szükségünk, mint összeszámolni az egymástól különböző megoldásokat.

(de part1 (File)
  (search File)
  (print (length (uniq *Paths))) )

# A második rész viszont az összes utat kéri, tehát ekkor el kell hagynunk az egyediséget.

(de part2 (File)
  (search File)
  (print (length *Paths)) )
