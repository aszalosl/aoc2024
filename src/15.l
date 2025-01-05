# A tizenötödik feladat ládatologatásról szól, majdnem sokoban.
# Szerencsére nem a stratégiát kell megkeresnünk, hanem a megadottat
# végrehajtani. Mivel mind a térkép, mind a lépések az inputban találhatóak,
# kicsit bonyolultabb a beolvasás, az első üres sornál váltani kell.
# Az egyszerűség kedvéért két globális változót fogunk használni.

(de load-data (File)
   (in File
      (let Line ""
         (setq *Table (make
               (until (= (set 'Line (line)) "")
                  (link Line ) ))))

      (setq *Moves (make
         (until (eof)
            (chain (line)) )))))

# Mivel a robot helye is a térképen lett megadva, azt onnan kell kinyerni.

(de find-robot (Map)
   (for (Y . Line) Map
      (T (member "@" Line) (list (index "@" Line) Y))))

# Miután - a sokoban-tól eltérően egyszerre nem csak egy dobozt tolhatunk el,
# egy listát állítunk össze a mozgatandó karakterekből és azok koordinátáiból.
# Az eltolás valójában annyit jelent, minden az eredeti pozíciójától eggyel
# odébb rakunk. Ezt az listánk elforgatásával oldjuk meg.

(de rotate-v (X Ys Qs)
   (rot Qs)
   (while Qs
      (let (C (pop 'Qs) Yc (pop 'Ys))
      (put *Table Yc X C))) )

# A függőleges mozgatások egy kaptafára mennek, a V változó jelöli az
# Y koordinátát a robottól egészen a falig - az X koordináta nem változik.
# Normális esetben dobáljuk a sorokként kezelt listákba a karaktereket és
# pozíciókat. Ha fallal találkozunk, akkor haszontalan volt a számolás, itt
# most nem lesz mozgás. Ha viszont üres helyre - pontra - akadunk, még ezt
# is eltároljuk, és jöhet a forgatás.

(de move-v (X Y Limit)
   (let (Qs Nil Ys Nil)
      (for V (range Y Limit)
         (T (= "#" (get *Table V X)) NIL)
         (T (= "." (get *Table V X))
            (queue 'Qs ".") (queue 'Ys V)
            (rotate-v X Ys Qs))
         (queue 'Qs (get *Table V X)) (queue 'Ys V) )))

# Csak az irányhoz tartozó fal Y koordinátáját kell megadnunk.

(de move-up (X Y)
   (move-v X Y 1))

(de move-down (X Y)
   (move-v X Y (length *Table)))

# A vízszintes forgatás hasonló, csak X és Y szerepet cserél.

(de rotate-h (Xs Y Qs)
   (rot Qs)
   (while Qs
      (let (C (pop 'Qs) Xc (pop 'Xs))
         (put *Table Y Xc C))) )


(de move-h (X Y Limit)
   (let (Qs Nil Xs Nil)
      (for U (range X Limit)
         (T (= "#" (get *Table Y U)) NIL)
         (T (= "." (get *Table Y U))
            (queue 'Qs ".") (queue 'Xs U)
            (rotate-h Xs Y Qs))
         (queue 'Qs (get *Table Y U)) (queue 'Xs U) )))

(de move-left (X Y)
   (move-h X Y 1))
(de move-right (X Y)
   (move-h X Y (length (car *Table))))

# A teszteléshez nem árt, ha az aktuális állapotot ki tudjuk rajzolni.

(de draw (Map)
   (for Line Map
      (prinl (pack Line)))
   (prinl) )

# Az eredmény kiszámolásához segédfüggvény vezetünk be.

(de gps ()
   (let Counter 0
      (for (Y . Line) *Table
         (for (X . Ch) Line
            (when (= Ch "O") (inc 'Counter (+ (* 100 (dec Y)) (dec X)))) ))
      Counter))

# Kezdetben fel kell dolgoznunk az inputot, majd a megadott lépéseket
# egy ciklussal járjuk be. Soron következő karakter dönti el, hogy melyik
# függvényt kell meghívnunk, illetve a robot pozíciójának karaktere,
# hogy sikeres volt a mozgás, vagy sem. Ha igen, akkor aktualizálni kell
# a robot pozícióját. Végül pedig ki kell számolni a végállapot "értékét".

(de part1 (File)
   (load-data File)
   # (draw *Table)
   (let (R (find-robot *Table) X (pop 'R) Y (pop 'R))
      (for C *Moves
         (when
            (= C "<")
            (move-left X Y)
            (when (= "." (get *Table Y X)) (dec 'X)))
         (when
            (= C "\^")
            (move-up X Y)
            (when (= "." (get *Table Y X)) (dec 'Y)))
         (when
            (= C ">")
            (move-right X Y)
            (when (= "." (get *Table Y X)) (inc 'X)))
         (when
            (= C "v")
            (move-down X Y)
            (when (= "." (get *Table Y X)) (inc 'Y)))
         # (draw *Table)
   ))
   (print (gps)) )

# Part 2
# A feladat második felében másképp kell értelmezni az inputot.
# Szó szerint követjük a szabályzatban leírtakat.
# Mivel listák listájáról van szó, első körben egy listát/sort dolgozunk fel.

(de double-line (Line)
   (let Dline
      [make
         (for Char Line
            (when (= "#" Char) (chain (list "#" "#")))
            (when (= "O" Char) (chain (list "[" "]")))
            (when (= "." Char) (chain (list "." ".")))
            (when (= "@" Char) (chain (list "@" "."))) ]
      Dline]

# Majd pedig az egész térképet.

(de double-map ()
   (setq *Table2
      (make
         (for Line *Table
            (link (double-line Line)) ]

# A vízszintes mozgatás lényegesen nem változott, viszont a függőlegesenél
# a téglakötés miatt hatalmas változások történnek, amit a kódnak is
# le kell kezelnie.
# Összeszedtük, hogy mely mezőket kell mozgatni. Előbb letöröljük a helyüket,
# majd a megfelelő irányban eggyel léptetve mind kirajzoljuk.

(de move-vert (Mss Up)
   (for Ms Mss  # clear
      (for M Ms
         (put *Table2 (cadr M) (car M) ".")))
   (if Up
      (for Ms Mss
         (for M Ms
            (put *Table2 (dec (cadr M)) (car M) (caddr M)) ))
      (for Ms Mss
         (for M Ms
            (put *Table2 (inc (cadr M)) (car M) (caddr M)) ) ]

# A mozgatandó dobozokat soronként dolgozzuk fel.
# Az utoljára összegyűjtött elemeken kell újra végigmenni, és nézni, hogy
# felette (vagy alatta) található-e doboz. Ha igen, azt is fel kell venni
# a mozgatandók közé, és persze a másik felét is.
# Ha falat akarunk mozgatni, akkor a legrövidebb úton lépjünk ki a függvényből.
# Ha nem gyűjtöttünk össze semmit a legutóbbi körben, akkor ideje meghívni az
# előző függvényt, és ebből pedig kilépni.

(de move-v2  (X Y Limit)
   (let (Moves (list (list (list X Y "@"))) Current Nil Blocked NIL)
      (for V (range Y Limit)
         (setq Current Nil)
         (for Node (car Moves)
            (let (
               Xi (car Node)
               Yi (if (=1 Limit) (dec V) (inc V))
               Char (get *Table2 Yi Xi))

               (when (= "#" Char) (setq Blocked T))
               (when (= "]" Char)
                  (push 'Current (list (dec Xi) Yi "["))
                  (push 'Current (list Xi Yi "]")) )
               (when (= "[" Char)
                  (push 'Current (list Xi Yi "["))
                  (push 'Current (list (inc Xi) Yi "]")) )
            ))
         (T Blocked NIL) # if blocked somewhere no movement
         (NIL Current # move and exit
            (move-vert Moves (=1 Limit)) )
         (push 'Moves (uniq Current)) ]

# Mivel direktben hivatkozunk a táblát tartalmazó globális változóra,
# a vízszintes mozgatás függvényeinek el kell készíteni a variánsait.

(de rotate-h2 (Xs Y Qs)
   (rot Qs)
   (while Qs
      (let (C (pop 'Qs) Xc (pop 'Xs))
         (put *Table2 Y Xc C))) )

(de move-h2 (X Y Limit)
   (let (Qs Nil Xs Nil)
      (for U (range X Limit)
         (T (= "#" (get *Table2 Y U)) NIL)
         (T (= "." (get *Table2 Y U))
            (queue 'Qs ".") (queue 'Xs U)
            (rotate-h2 Xs Y Qs))
         (queue 'Qs (get *Table2 Y U)) (queue 'Xs U) )))

(de move-left2 (X Y)
   (move-h2 X Y 1))
(de move-right2 (X Y)
   (move-h2 X Y (length (car *Table2))))

# Lényegében ez igaz a végeredmény "értékére" is.

(de gps2 ()
   (let Counter 0
      (for (Y . Line) *Table2
         (let DY (- Y 1)
            (for (X . Ch) Line
               (let DX (- X 1)
                  (when (= Ch "[")
                     (inc 'Counter (+ (* 100 DY) DX)) )))))
      Counter))

# Természetesen fel kell használni a korábban szereplő függvényeket,
# de az elvek ugyanazok, mint az első esetben.

(de part2 (File)
   (load-data File)
   (double-map)
   # (draw *Table2)
   (let (R (find-robot *Table2) X (pop 'R) Y (pop 'R))
      (for C *Moves
         (when
            (= C "<")
            (move-left2 X Y)
            (when (= "." (get *Table2 Y X)) (dec 'X)) )
         (when
            (= C "\^")
            (move-v2 X Y 1)
            (when (= "." (get *Table2 Y X)) (dec 'Y) ))
         (when
            (= C ">")
            (move-right2 X Y)
            (when (= "." (get *Table2 Y X)) (inc 'X)))
         (when
            (= C "v")
            (move-v2 X Y (length *Table2))
            (when (= "." (get *Table2 Y X)) (inc 'Y) ))
         # (draw *Table2)
   ))
   (print (gps2)) )