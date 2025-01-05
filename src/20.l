#A huszadik feladatban csalási lehetőségeket kell felfedezni. Adott egy térkép,
#melyen van egy egyértelmű útvonal, s keresni kell, hogy milyen rövidítések
#léteznek, ha a pályát elhagyhatjuk egy lépésben.
# A térképről be kell gyűjteni a kezdő és cél pozíciókat.

(de find-ends ()
   (for (Y . Line) *Table
      (for (X . Ch) Line
         (when (= Ch "S") (setq *Sx X *Sy Y))
         (when (= Ch "E") (setq *Ex X *Ey Y]

# Mint mindig, most is be kell gyűjteni a térképet. Annak megfelelően elkészíjük
# egy külön változóban a verseny útvonalát. S ha már ezek globális változók
# lesznek, akkor itt megadjuk a további globális változóinkat is.

(de input-data (File)
   (in File
      (setq
         *Empty -1
         *Limit 100
         *Counter 0
         *Table
         (make
            (until (eof)
               (link (line))))
         *Map
         (make (for J (length *Table)
            (link (make
               (for I (length (car *Table))
               (link *Empty]

# Annak érdekében, hogy észrevegyük a (le)rövidítéseket, meg kell alkotni az
# útvonalat. Ezt most iteratív módon oldjuk meg: ha valamely irányban szabad
# az út (az inputban ott pont szerepelt), s még nem jártunk arra
# (nem jegyeztünk fel róla semmit a *Map-en), akkor azt megjelöljük.
# Vigyázni kell arra, hogy és mikor lépünk ki a végtelen ciklusból,
# mert fel kell dolgozni a célt is.

(de forward (Fringe)
   (loop
      (T (= NIL (car Fringe))
         (put *Map *Ey *Ex (inc (max
            (get *Map (dec *Ey) *Ex) (get *Map (inc *Ey) *Ex)
            (get *Map *Ey (dec *Ex)) (get *Map *Ey (inc *Ex)) ))))
      (let (
         Node (pop 'Fringe)
         Y (car Node) X  (cadr Node)
         V (get *Map Y X)
         Xm (dec X) Xp (inc X) Ym (dec Y) Yp (inc Y) Vp (inc V))
         (when (and (= "." (get *Table Y Xm)) (= *Empty (get *Map Y Xm)))
            (put *Map Y Xm Vp) (queue 'Fringe (list Y Xm)))
         (when (and (= "." (get *Table Y Xp)) (= *Empty (get *Map Y Xp)))
            (put *Map Y Xp Vp) (queue 'Fringe (list Y Xp)))
         (when (and (= "." (get *Table Ym X)) (= *Empty (get *Map Ym X)))
            (put *Map Ym X Vp) (queue 'Fringe (list Ym X)))
         (when (and (= "." (get *Table Yp X)) (= *Empty (get *Map Yp X)))
            (put *Map Yp X Vp) (queue 'Fringe (list Yp X))))))

# A feladat kiírása szerint két lépés után újra a pályán kell lennünk, tehát
# átlós lépés nem fér bele. Ezért a négy égtáj irányába dupla lépést vizsgálunk:
# onnan az aktuális helyzetbe a feladat szempontjából érdekes rövidítéssel
# jutottunk? Ha igen, akkor a globális változót növeljük.

(de check-neighbours (X Y)
   (let (Xm (- X 2) Xp (+ X 2) Ym (- Y 2) Yp (+ Y 2) V (get *Map Y X)
      Vmn (get *Map Y Xm) Vpn (get *Map Y Xp)
      Vnm (get *Map Ym X) Vnp (get *Map Yp X))
      (when
         (and (not (= Vmn NIL)) (<> Vmn *Empty) (<= (+ Vmn *Limit 2) V))
         (inc '*Counter))
      (when
         (and (not (= Vpn NIL)) (<> Vpn *Empty) (<= (+ Vpn *Limit 2) V))
         (inc '*Counter))
      (when
         (and (not (= Vnm NIL)) (<> Vnm *Empty) (<= (+ Vnm *Limit 2) V))
         (inc '*Counter))
      (when
         (and (not (= Vnp NIL)) (<> Vnp *Empty) (<= (+ Vnp *Limit 2) V))
         (inc '*Counter))
      ))

# Páran a versenypályát követve oldották meg a feladatot, de mivel úgyis csak
# egy darabszám érdekel bennünket, végigdaráljuk az egész táblát. Pontosabban a
# széleket figyelmen kívül hagyjuk, ott úgy sem vezet a pálya.

(de count-cheats ()
   (for Y (range 2 (length *Map))
      (for X (range 2 (length (car *Map)))
         (when (<> *Empty (get *Map Y X)) (check-neighbours X Y]

# A futáshoz természetesen be kell olvasni a feladat inputját, megkeresni a
# térképen a star és cél pozícióját, megalkotni az útvonalat, és megkeresni a
# csalási lehetőséget.

(de part1 (File)
   (input-data File)
   (find-ends)
   (put *Map *Sy *Sx 0)
   (forward (list (list *Sy *Sx)))
   (count-cheats)
   (println *Counter)
   )

# Part 2

# A feladat második felében csak a csalás hossza változott, így a 2 sugarú
# környezet helyett 20 sugarúval kell dolgozni.

(setq *CheatSize 20)

# Persze itt már nem fogjuk egyesével felsorolni és tesztelni a környezeteket,
# Hanem egy dupla ciklussal megalkotjuk a Manhattan távolságot.
# Fontos, hogy a két vizsgált pont (X/Y és Nx/Ny) egyaránt rajta legyen a táblán,
# sőt a verseny útvonalán is. Ha még a rövidítéssel kapcsolatos feltételnek is
# eleget tesznek, akkor növelhetjük a számlálót.

(de check-neighbours2 (X Y)
   (for Dy (range (- *CheatSize) *CheatSize)
      (let DLimit (- *CheatSize (abs Dy))
         (for Dx (range (- DLimit) DLimit)
            (let (Nx (+ X Dx) Ny (+ Y Dy))
               (when (and
                  (< 1 Ny (length *Map))
                  (< 1 Nx (length (car *Map))))
                  (let ( V (get *Map Y X) VD (get *Map Ny Nx))
                     (when
                        (and (<> VD *Empty) (<= (+ VD *Limit (abs Dx) (abs Dy)) V))
                        (inc '*Counter) ))))))))

# Az alábbi két függvényben semmi újdonság nincs, csupán az újabb verziójú
# függvényeket hívják meg.

(de count-cheats2 ()
   (for Y (range 2 (length *Map))
      (for X (range 2 (length (car *Map)))
         (when (<> *Empty (get *Map Y X)) (check-neighbours2 X Y]

(de part2 (File)
   (input-data File)
   (find-ends)
   (put *Map *Sy *Sx 0)
   (forward (list (list *Sy *Sx)))
   (count-cheats2)
   (println *Counter)
   )
