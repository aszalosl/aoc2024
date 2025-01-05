# A lineáris algebra folytatódik a következő feladatban is, csupán bejön
# a maradékszámítás is. Mivel több számot kell begyűjteni egy sorból,
# kicsit utánanéztem a lehetőségeknek. Ha reguláris kifejezés nincs is,
# de valami azért megközelíti. Nem a legrövidebb, de azért használható.

(de to-number (X)
   (format (pack X)))

(de read-input (FILE)
   (in FILE
      (make
         (until (eof)
            (let Line (line)
               (use (@A @B @C @D @P @V)  # local vars
                  (and
                     (match '(@P "=" @A "," @B " " @V "=" @C "," @D) Line)
                     (link (list
                        (to-number @A)
                        (to-number @B)
                        (to-number @C)
                        (to-number @D))))))))))

# Más konstansokat használ a mintapélda, mint a teszt.
# Tároljuk mindkettőt, s használjuk azt, melyre szükség van!

# (setq *SizeX 11 *SizeY 7 *Time 100) # for the test case
(setq *SizeX 101 *SizeY 103 *Time 100) # for the real puzzle

# Ugyanazt a számolást kell végrehajtani az összes számnégyesre.
# Ahelyett hogy for ciklust használnék, kezeljük vektorokként
# az adatainkat. Mivel a maradék negatív számoknál negatív lesz,
# kicsit megbuheráljuk a számítást.
# Végül a kapott adatok feldolgozására csak használunk egy ciklust.

(de part1 (File)
   (let (
      Input (read-input File)
      Xs (mapcar car Input)
      Ys (mapcar cadr Input)
      Dxs (mapcar + (mapcar caddr Input) *SizeX)
      Dys (mapcar + (mapcar cadddr Input) *SizeY)
      Us (mapcar % (mapcar + Xs (mapcar '((Z) (* Z *Time)) Dxs)) *SizeX)
      Vs (mapcar % (mapcar + Ys (mapcar '((Z) (* Z *Time)) Dys)) *SizeY)
      Q1 0 Q2 0 Q3 0 Q4 0 HalfX (/ *SizeX 2) HalfY (/ *SizeY 2) )
      (for I (length Us)
         (let (U (get Us I) V (get Vs I))
            (when (and (< U HalfX) (< V HalfY)) (inc 'Q1))
            (when (and (< U HalfX) (> V HalfY)) (inc 'Q2))
            (when (and (> U HalfX) (< V HalfY)) (inc 'Q3))
            (when (and (> U HalfX) (> V HalfY)) (inc 'Q4))))
      (println (* Q1 Q2 Q3 Q4))))

# A második esetben a térképen valamikor megjelenik egy fenyőfa.
# Ezt az időpillanatot kell megtalálni.
# Kitolás, hogy nem tudjuk, hogy a térkép melyik részén kell keresni.
# Ezért valami eltolás-invariáns jellemzőt kell keresni.
# Egyik ilyen lehet a szomszédok száma, azaz olyan esetek száma, amikor
# két robot szomszédos. Az igazat megvallva, ezt duplán fogjuk számolni.
# Annak érdekében, hogy könnyen tudjunk szomszédokat keresni, az első
# résztől eltérően használjuk fel az inputot, nem oszloponként,
# hanem soronként dolgozzuk fel, s adott időponthoz megkeressük a
# szóban forgó robot pozícióját.

(de calc-place (Time X Y U V)
   (list
      (% (+ (% (+ X (* Time U)) *SizeX) *SizeX) *SizeX)
      (% (+ (% (+ Y (* Time V)) *SizeY) *SizeY) *SizeY)))

# Miután adott az összes robot pozíciójának listája, minden egyes pozíciónak
# meg kell vizsgálni a környezetét. Számunkra a négy égtáj elég lesz,
# felesleges nyolc irányt tesztelni. Természesen minden találatot feljegyzünk.

(de neighbours (Ps)
   (let Counter 0
      (for P Ps
         (let (X (car P) Y (cadr P))
            (when (member (list (dec X) Y) Points) (inc 'Counter))
            (when (member (list (inc X) Y) Points) (inc 'Counter))
            (when (member (list X (dec Y)) Points) (inc 'Counter))
            (when (member (list X (inc Y)) Points) (inc 'Counter))))
      Counter))

# Miután beolvastuk az inputot, reménykedünk, hogy nem milliós lépésszámmal
# kell dolgozni. Elinditunk egy ciklust a lépésszámra, meghatározzuk, hogy
# ekkor hova kerülnek a robotot, mekkora a szomszédsági mérőszám.
# A sejtésünk az, hogy a maximális értékénél alakul ki a kép, ezért ha elérjük,
# vagy túllépjük az addigi maximumot, akkor kiírjuk az indexet és a mérőszámot.

(de part2 (File)
   (let (Input (read-input File) Max 0)
      (for I 20000
         (let (
            Points
               (make
                  (for P Input
                     (link (calc-place I (car P) (cadr P) (caddr P) (cadddr P)))))
            Neighbours
               (neighbours Points))
            (when (>= Neighbours Max)
               (println I " " Neighbours)
               (setq Max Neighbours))))))

# Persze csak akkor lehetünk biztosak a megoldásban, ha látjuk is.
# Ehhez generálunk egy üres térképet - azaz listák listáját.

(de generate-table (X Y)
   (let Table
      (make
         (for I Y
            (link (range 1 X))))
      (for I X
         (for J Y
            (put Table J I ".")))
      Table))

# Majd az adott időponthoz generált pozíciókat megjelöljük. A belső listákból
# stringeket készítünk, és kiírjuk.

(de draw (File Time)
   (let (
      Input (read-input File)
      Points (make
         (for P Input
            (link (calc-place Time (car P) (cadr P) (caddr P) (cadddr P)))))
      # Rpoints (sort Points)
      Table (generate-table (inc *SizeY) (inc *SizeX)) )
      # (println Rpoints)
      (for P Points
         (put Table (inc (cadr P)) (inc (car P)) "#"))
      (for Line Table
         (prinl (pack Line)))))
