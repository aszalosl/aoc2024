# A rejtvényfejtők visszajelzései szerint idén a huszonegyedik feladat volt
# a legnehezebb. Annak érdekében, hogy ne vesztegessem el az időt a zsákutcákkal,
# telerakom a kódot egységtesztekkel, hogy már egyből látszódjon,
# ha valami esetleg félrement.
# Két billentyűzet szerepel a feladatban. Mindkettőben a billentyűket
# koordinátákkal azonosítjuk, és asszociatív tömbökben tároljuk a kordináták
# és feliratok párosait.

(setq *Numeric '( ((-1 0) . "0") ((0 0) . "a")
   ((-2 1) . "1") ((-1 1) . "2") ((0 1) . "3")
   ((-2 2) . "4") ((-1 2) . "5") ((0 2) . "6")
   ((-2 3) . "7") ((-1 3) . "8") ((0 3) . "9")))

# A kurzorbillenytűk esetén hasonlóan járunk el

(setq *Directional '( ((-1 0) . "u") ((0 0) . "a")
   ((-2 -1) . "l") ((-1 -1) . "d") ((0 -1) . "r")))


# A számbillentyűt kezelő robotot az alábbi függvény szimulálja.
# Ez a bejövő irányokat fordítja le számokra. Csupán azt kell összeírnunk, hogy
# az "Enter"-t jelentő "A" gomb lenyomásakor merre volt a robot karja.

(de command (L Keyboard)
   (let (Result NIL X 0 Y 0)
      (for C L
         (when (= C "u") (inc 'Y))
         (when (= C "d") (dec 'Y))
         (when (= C "r") (inc 'X))
         (when (= C "l") (dec 'X))
         (when (= C "a") (queue 'Result (cdr (assoc (list X Y) Keyboard)))) )
      Result))

# Annak érdekében, hogy a tesztekben ne írjunk el, és feleslegesen ne
# ismételjünk semmit, bevezetünk jó pár globális változót.
# Mivel a kalap karaktert a PicoLisp nem igazán szereti, lecseréltem az összes
# kurzorfeliratot a neki megfelelő angol szó kezdőbetűjére, s ha már mind
# kisbetű, az "A"-ból is "a" lett.

(setq
   *L29 (chop '029a)
   *L980 (chop '980a)
   *L179 (chop '179a)
   *L456 (chop '456a)
   *L379 (chop '379a)
   *N29-1 (chop 'lauaruuaddda)
   *N29-2 (chop 'lauauruaddda)
   *N29-3 (chop 'lauauuraddda)
   *DN29 (chop 'dllarrualaradaluaaraldaaarua)
   *DDN29 (chop 'ldalaarruadaaluaraldlarruadaualdarualdlaruaraadaualdlararuaaadaluara)
   *DDN980 (chop 'ldlarruaaadaualdalaarruadaaluaraldlararuaaadaluaraldarualara)
   *DDN179 (chop 'ldlarrualdalarruaadaaluaraldlarruaadaualdaruaalaraldlararuaaadaluara)
   *DDN456 (chop 'ldlarruaaldalarruaadaaluaraldarualaraldarualaraldlararuaadaluara)
   *DDN379 (chop 'ldlarruadaualdalaarruaadaluaraadaualdaruaalaraldlararuaaadaluara)
               )

# Így néznek ki az egységtesztek. Ha a két kifejezés nem egyezik meg, a fájl
# beolvasásakor/frissítésekor a rendszer már hibát jelez.

(test *L29 (command *N29-1 *Numeric))
(test *L29 (command *N29-2 *Numeric))
(test *L29 (command *N29-3 *Numeric))
(test *N29-1 (command *DN29 *Directional))
(test *L29 (command (command *DN29 *Directional) *Numeric))

# Ezzel elegendő eszközünk van, hogy szimuláljuk a rendszer működését.
# A feladat megoldásához a pont szimuláció ellentettjére van szükség.
# Míg a szimuláció megadja, hogy adott inputra milyen outputot kapunk,
# nekünk pont arra van szükség, hogy adott outputot milyen inputtal nyerhetünk.
# Viszont amint az a feladat leírásából kiderül, hogy míg a command
# matematikailag is függvény, az inverze már nem. Nekünk pedig a sok lehetséges
# input közül a legrövidebbet kell meglelni. Nem más ez, mint egy dinamikus
# programozási feladat. Ezért kezeljük is úgy!

# Míg az előbb arra volt szükség, hogy adott koordinátán melyik billentyű
# található, most fordítva kell használni az asszociatív tömböt/listát, azaz
# a billentyű koordinátáit kell visszadni.

 (de pos-dir (Char Keyboard)
    (car (rassoc Char Keyboard)))

# Alapvetően az kérdés, hogy mennyit kell vándorolni a billentyűzeteken.
# Ehhez készítünk egy távolságfüggvényt, ami viszont nem fogja teljesíteni a
# metrika tulajdonságait, pl. nem lesz szimmetrikus!
# Tehát adott két karakter, ennek távolságát kell visszadnunk különféle
# távolságfogalmak és billentyűzetek esetén.
# Igen, megjelenik az absztrakció, amellyel sok programozó nem tud mit kezdeni.
# Erre még visszatérünk hamarosan.

 (de distance (Char1 Char2 Function Keyboard)
    (let (
       Pos1 (pos-dir Char1 Keyboard)
       Pos2 (pos-dir Char2 Keyboard)
       Args (list
             (car Pos1) (cadr Pos1)
             (car Pos2) (cadr Pos2) ))
      (apply Function Args) ))

# A helyzet, hogy két billenytű között igen sok úton közlekedhetünk.
# Mondhatnánk, hogy minden variációt érdemes figyelembe venni, de nem lenne
# igazunk, mert permutációkról van szó, abból is az ismétlésesről.
# Valójában két irány között vacilálhatunk, ezeket jelöli az A és a B,
# míg az, hogy melyiket hányszor kell használni a C és D írja le.
# Ha már egyikből sincs egy sem, akkor az enter-nek megfelelő "A" kódját írjuk le
# a sorozat végére. Az alábbi rutin elkészíti az összes lehetséges permutációt,
# viszont egyik sorozatot sem ismétli meg!

(de perm (A B C D)
   (cond
      ((= A B 0) (list (list "a")))
      ((= A 0) (mapcar '((X)(cons D X)) (perm 0 (dec B) C D)))
      ((= B 0) (mapcar '((X)(cons C X)) (perm (dec A ) 0 C D)))
      (T (append
         (mapcar '((X)(cons C X)) (perm (dec A ) B C D))
         (mapcar '((X)(cons D X)) (perm A (dec B) C D))))))

# A legegyszerűbb esetben - amikor direkt irányítunk egy robotot, csak a
# Manhattan távolságra van szükség, de ne feledkezzük el a lezáró Enter-ről se!

(de distance1 (X Y U V)
   (+ (abs (- X U)) (abs (- Y V)) 1))

# Ha egy kódot kell bepötyögni, akkor egymás után meg kell adni a betűit, azaz
# a teljes költséghez ki kell számolni az egymást követő betűk távolságát.
# Ne feledjük, az Enter-től indulunk. Itt már felhasználunk valamilyen
# távolságfogalmat.

(de path-cost (Path Distance)
   (let (Ch "a" Cost 0)
      (for L Path
         (inc 'Cost (cdr (assoc (list Ch L) Distance)))
         (setq Ch L))
      Cost))

# És most pontosítjuk, hogy elsőfajú, másodfajú és harmadfajú távolságon mit is
# értünk. A D1-D3 az a távolságok táblázata.

(de path-cost1 (Path)
   (path-cost Path *D1))

(de path-cost2 (Path)
   (path-cost Path *D2))

(de path-cost3 (Path)
   (path-cost Path *D3))

# Van egy átverés/szívatás a feladatban, egyik robotkéz sem hagyhatja el a
# billentyűzetet. Az Enter-től kettőre balra mindkét billenytűzeten egy lyuk
# van, ami tiltott terület. Azaz bármely mozgássorozatnál tesztelni kell,
# hogy nem keveredik ide valamely robot. Kicsit körülményes kiugrani
#  a ciklusból, de utána érdemes tesztelni, hogy lefutott a ciklus, vagy
# félbeszakadt.

(de safe-path (Path X Y)
   (for P Path
      (when (= P "u") (inc 'Y))
      (when (= P "d") (dec 'Y))
      (when (= P "l") (dec 'X))
      (when (= P "r") (inc 'X))
      (T (and (= Y 0) (= X -2)) NIL)
      )
   (if (and (= Y 0) (= X -2)) NIL T))

# Itt generáljuk a permutációhoz szükséges adatokat. A két koordinátapár
# elhelyezkedésétől függően balra/jobbra illetve fel/le kell haladni.
# A vízszintes és függőleges távolságok megadják az ismétlések számát.
# Majd elkészül az összes permutáció, melyekből kirostáljuk azokat, amelyek
# tiltott helyre keverednek el.

(de paths (X Y Z W)
   (let (
      V (if (> W Y) "u" "d")  # vertical
      H (if (> Z X) "r" "l")  # horizontal
      Dv (abs (- W Y))        # v-distance
      Dh (abs (- Z X))        # h-distance
      Paths (perm Dv Dh V H)
      SafePaths (filter '((P)(safe-path P X Y)) Paths))
      SafePaths))

# Míg a distance1 volt a direkt irányított robothoz kapcsolódó távolság,
# a robot által irányított robothoz már többet kell dolgoznunk.
# Az előbbi függvény megadja a lehetséges útvonalakat, melyek mindegyikére
# alkalmazzuk a kód költségét, és ebből a minimális értéket vesszük.
# Bennünket csak azért sem a konkrét billenytűsorozat érdekel, hanem annak
# a hossza.

(de distance2 (X Y Z W)
   (let Paths (paths X Y Z W)
      (mini path-cost1 Paths)
      @@))

# Az, hogy az indirekciót eggyel növeljük, csak azt jelenti, hogy más
# távolságfüggvényt kell használni, pont azt, amelyet az előbb alkottunk meg.

(de distance3 (X Y Z W)
   (let Paths (paths X Y Z W)
      (mini path-cost2 Paths)
      @@))

# Most már minden adott, hogy rendre kiszámoljuk a távolságtáblázatainkat.
# Az első két esetben kurzorbillentyűkre, míg az utolsó esetben - a legnagyobb
# indirekciónál számbillentyűzetre.

(setq *D1
   (let Keys (mapcar cdr *Directional)
      (make
         (for A Keys
            (for B Keys
               (link
                  (cons
                     (list A B)
                     (distance A B distance1 *Directional) )))))))

(setq *D2
   (let Keys (mapcar cdr *Directional)
      (make
         (for A Keys
            (for B Keys
               (link
                  (cons
                     (list A B)
                     (distance A B distance2 *Directional))) )))))

(setq *D3
   (let Keys (mapcar cdr *Numeric)
      (make
         (for A Keys
            (for B Keys
               (link
                  (cons
                     (list A B)
                     (distance A B distance3 *Numeric))) )))))

# Mielőtt nekiállnánk a saját feladatunk megoldásának, lássuk, hogy
# jól működik a programunk a leírásban megadottakra.

(test (path-cost3 (chop '029a)) (length *DDN29))
(test (path-cost3 (chop '980a)) (length *DDN980))
(test (path-cost3 (chop '179a)) (length *DDN179))
(test (path-cost3 (chop '456a)) (length *DDN456))
(test (path-cost3 (chop '379a)) (length *DDN379))

# Ha igen, akkor nekieshetünk a saját problémánknak is.

(de code (N)
   (let LN (chop N)
      (* (format (pack (head 3 LN))) (path-cost3 LN))))

(de part1 (L)
   (apply + (mapcar code L)))

# Természetesen nem itt a saját adatokat kellene szerepeltetni.
# Ám öt számért az ember nem ír egy beolvasó rutint ;-)

(println (part1 (list '029a '980a '179a '456a '379a)))


# Part 2
# A feladat második fele semmi újat nem hoz be, csak az indirekciót tekeri
# maximumra. Az előbbi kétdimenziós táblázatok helyett be lehetne hozni egy
# háromdimenziósat, de majdnem ugyanannyi időbe kerül egy jó szövegszerkesztővel
# lemásolni mindent. Az interpreter miatt annyira nem lényeges, hogy mit mikor
# definiálunk, akkor legyen meg, ha felhaszáljuk!

(setq *DKeys (mapcar cdr *Directional) *NKeys (mapcar cdr *Numeric) )
(de path-cost01 (Path) (path-cost Path *D01))
(de path-cost02 (Path) (path-cost Path *D02))
(de path-cost03 (Path) (path-cost Path *D03))
(de path-cost04 (Path) (path-cost Path *D04))
(de path-cost05 (Path) (path-cost Path *D05))
(de path-cost06 (Path) (path-cost Path *D06))
(de path-cost07 (Path) (path-cost Path *D07))
(de path-cost08 (Path) (path-cost Path *D08))
(de path-cost09 (Path) (path-cost Path *D09))
(de path-cost10 (Path) (path-cost Path *D10))
(de path-cost11 (Path) (path-cost Path *D11))
(de path-cost12 (Path) (path-cost Path *D12))
(de path-cost13 (Path) (path-cost Path *D13))
(de path-cost14 (Path) (path-cost Path *D14))
(de path-cost15 (Path) (path-cost Path *D15))
(de path-cost16 (Path) (path-cost Path *D16))
(de path-cost17 (Path) (path-cost Path *D17))
(de path-cost18 (Path) (path-cost Path *D18))
(de path-cost19 (Path) (path-cost Path *D19))
(de path-cost20 (Path) (path-cost Path *D20))
(de path-cost21 (Path) (path-cost Path *D21))
(de path-cost22 (Path) (path-cost Path *D22))
(de path-cost23 (Path) (path-cost Path *D23))
(de path-cost24 (Path) (path-cost Path *D24))
(de path-cost25 (Path) (path-cost Path *D25))
(de path-cost26 (Path) (path-cost Path *D26))

(de distance02 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost01 Paths) @@))
(de distance03 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost02 Paths) @@))
(de distance04 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost03 Paths) @@))
(de distance05 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost04 Paths) @@))
(de distance06 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost05 Paths) @@))
(de distance07 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost06 Paths) @@))
(de distance08 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost07 Paths) @@))
(de distance09 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost08 Paths) @@))
(de distance10 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost09 Paths) @@))
(de distance11 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost10 Paths) @@))
(de distance12 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost11 Paths) @@))
(de distance13 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost12 Paths) @@))
(de distance14 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost13 Paths) @@))
(de distance15 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost14 Paths) @@))
(de distance16 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost15 Paths) @@))
(de distance17 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost16 Paths) @@))
(de distance18 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost17 Paths) @@))
(de distance19 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost18 Paths) @@))
(de distance20 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost19 Paths) @@))
(de distance21 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost20 Paths) @@))
(de distance22 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost21 Paths) @@))
(de distance23 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost22 Paths) @@))
(de distance24 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost23 Paths) @@))
(de distance25 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost24 Paths) @@))
(de distance26 (X Y Z W) (let Paths (paths X Y Z W) (mini path-cost25 Paths) @@))

(setq *D01 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance1 *Directional)))))))
(setq *D02 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance02 *Directional)))))))
(setq *D03 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance03 *Directional)))))))
(setq *D04 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance04 *Directional)))))))
(setq *D05 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance05 *Directional)))))))
(setq *D06 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance06 *Directional)))))))
(setq *D07 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance07 *Directional)))))))
(setq *D08 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance08 *Directional)))))))
(setq *D09 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance09 *Directional)))))))
(setq *D10 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance10 *Directional)))))))
(setq *D11 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance11 *Directional)))))))
(setq *D12 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance12 *Directional)))))))
(setq *D13 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance13 *Directional)))))))
(setq *D14 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance14 *Directional)))))))
(setq *D15 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance15 *Directional)))))))
(setq *D16 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance16 *Directional)))))))
(setq *D17 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance17 *Directional)))))))
(setq *D18 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance18 *Directional)))))))
(setq *D19 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance19 *Directional)))))))
(setq *D20 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance20 *Directional)))))))
(setq *D21 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance21 *Directional)))))))
(setq *D22 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance22 *Directional)))))))
(setq *D23 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance23 *Directional)))))))
(setq *D24 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance24 *Directional)))))))
(setq *D25 (make (for A *DKeys (for B *DKeys (link (cons (list A B) (distance A B distance25 *Directional)))))))
(setq *D26 (make (for A *NKeys (for B *NKeys (link (cons (list A B) (distance A B distance26 *Numeric)))))))

# Arra figyeljünk, hogy ne a harmadik, hanem a huszonhatodik távolságot
# használjuk! Ettől eltekintve a maradék ugyanaz mint korábban.

(de code2 (N)
   (let LN (chop N)
      (* (format (pack (head 3 LN))) (path-cost26 LN))))

(de part2 (L)
   (apply + (mapcar code2 L)))

(println (part2 (list '029a '980a '179a '456a '379a)))
