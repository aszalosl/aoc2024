# A negyedik feladatban szókereséssel kell foglalkoznunk.
# Az inputot sorok listájaként értelmezzük. Ebben nyolc irányban állhat a keresett szó.
# Ebből négyhez alkotunk külön-külön függvényeket, s benne oda-vissza keressük a szavunkat.

(de input (File)
   (in File
      (make
         (until (eof)
            (link (line))))))

(de beginsWithXmas (L)
   (if (or
         (and (= (car L) "X") (= (cadr L) "M") (= (caddr L) "A") (= (cadddr L) "S"))
         (and (= (car L) "S") (= (cadr L) "A") (= (caddr L) "M") (= (cadddr L) "X")) )
      1 0))

# A soronkénti kereséshez a maplist segítségével leszalámizzuk a sort, s csak az eredményül kapott
# listák elejét kell figyelemmel követni. Persze ezt minden soron végig kell futtatni, amire a mapcar használható.
(de count_line (L)
   (apply + (maplist beginsWithXmas L)))

(de count_by_lines (LL)
   (apply + (mapcar count_line LL)))

# Az oszloponkénti vizsgálathoz leválasztjuk a sorok első tagját - azaz az első oszlopot -,
# és ezt egy sorként kezeljük, azaz az előbb látott módszerrel végigszkenneljük a szó után.
(de count_by_columns (LL C)
   (if (caar LL)
      (count_by_columns
         (mapcar cdr LL)
         (+ C (count_line (mapcar car LL))))
      C))

# Ezzel kész is vagyunk a vízszintes és függőleges irányokkal, jöjjenek az átlósak!
# Ehhez most nem az egész táblázattal dolgozunk, csak az első négy sorával, és abból is lecsippentjük
# azok pár kezdő elemét, ügyelve arra, hogy a keresett átló pont függőleges legyen.
# Itt pedig már egyértelmű lesz, hogy minek kell majd a sorok elején állni.

(de truncate (L)
   (list (cdddar L) (cddadr L) (cdaddr L) (cadddr L)))

(de count_by_trunc (R C)
   (if (caar R)
      (let FC (mapcar car R)
         (if (or (= FC (list "X" "M" "A" "S")) (= FC (list "S" "A" "M" "X")))
            (count_by_trunc (mapcar cdr R) (inc C))
            (count_by_trunc (mapcar cdr R) C)))
      C))

# A maplist segítségével megint leszalámizhatjuk a sorainkat - bár elég lenne minden egymást követő négy sor,
# melyeket az előbbi rutinokkal ellenőrzünk.

(de make-it-easy (L)
   (count_by_trunc (truncate L) 0))

(de count_by_diagonal (L)
   (apply + (maplist make-it-easy L)))

# Végül nincs más dolgunk, mint a négy irány eredményeit összeadjuk. (A negyedik irány a harmadikból egyszerű tükrözéssel kinyerhető.)
(de countXmas (L)
   (+
      (count_by_lines L)
      (count_by_columns L 0)
      (count_by_diagonal L)
      (count_by_diagonal (mapcar reverse L))))

(de part1 (File)
   (let Table (input File)
      (prinl (countXmas Table))))

# part 2
# A feladat második felében a fő problémát a szójáték megfejtése okozhatta, szerencsére előttem már ezrek
# megoldották a feladatot, és figyelmeztették a többieket erre.
# Most egy keresztet kell keresnünk, ehhez hármas részekben kell átfésülni a sorokat, majdnem úgy,
# mint azt az átlós esetben tettük. A kereszt vizsgálatához különféle lista-elem predikátumokat kellett felhasználni.
# Az öt pozíciót öt változóval (A-E) kezeltük le.

(de isXmas (L)
   (let (A (caar L) B (caddar L) C (cadadr L) D (caaddr L) E (cadr (cdaddr L)))
      (and
         (= C "A")
         (or (and (= A "M") (= E "S")) (and (= A "S") (= E "M")))
         (or (and (= B "M") (= D "S")) (and (= B "S") (= D "M"))) )))

(de countX (L C)
   (if (caddar L)
      (if (isXmas L)
         (countX (mapcar cdr L) (inc C))
         (countX (mapcar cdr L) C))
      C))

(de sg2 (L)
   (countX (head 3 L) 0))

(de part2 (File)
   (let Table (input File)
      (apply + (maplist sg2 Table))))
