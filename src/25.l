# Az utolsó feladatban zárakat és kulcsokat kell összeilleszteni.
# Az input grafikusan lett megadva, hét sornyi szövegben az oszlopokat
# kell beazonosítani. Mivel százszámra vannak ilyen hetes csoportok, elsőre
# ezeket szedjük össze. Egy csoport listák listája lesz, a teljes feladat
# inputja pedig ezek listája.

(de read-input (File)
   (let (Groups NIL Lines NIL Line NIL)
      (in File
         (until (eof)
            (setq Line (line))
            (if (= Line NIL)
               (setq Groups (cons Lines Groups) Lines NIL)
               (queue 'Lines Line))))

      (cons Lines Groups)))

# A következő lépésben azt számoljuk meg, hogy egy oszlopban hány mező
# van kitöltve. Lehet, hogy a mátrix-ként kezelt listák listáját szellemesebb
# lenne transzponálni, s aztán feldolgozni a sorokat, ám mivel csak öt
# oszlopunk van, a copy-paste-vel én gyorsabban megírtam a programot.
# (Utánanéztem, hogy a nagymenők hogyan oldották ezt a transzponálást.
# https://rosettacode.org/wiki/Matrix_transposition
# Ennél jóval rövidebbeb, de szerintem nekem kell egy-két óra, amíg felfogom
# azt a megoldást.) Nem figyeltem arra, hogy összefüggőek-e az oszlopok, csak
# a darabszámra mentem.

(de summarize (Group)
   (let (C1 0 C2 0 C3 0 C4 0 C5 0)
      (for Line Group
         (when (= "#" (car Line)) (inc 'C1))
         (when (= "#" (cadr Line)) (inc 'C2))
         (when (= "#" (caddr Line)) (inc 'C3))
         (when (= "#" (cadddr Line)) (inc 'C4))
         (when (= "#" (get Line 5)) (inc 'C5)))
      (list C1 C2 C3 C4 C5)))

# Valahogy szét kell válogatni a kulcsokat a záraktól. Ehhez az első sor első
# karakterét kell figyelni. Össze lehetett volna vonni az alábbi két függvényt,
# de talán így olvashatóbb.

(de locks (Groups)
   (let Locks NIL
      (for Group Groups
         (when (= "#" (caar Group))
            (push 'Locks (summarize Group))))
      Locks))

(de keys (Groups)
   (let Keys NIL
      (for Group Groups
         (when (= "." (caar Group))
            (push 'Keys (summarize Group))))
      Keys))

# Itt vagyunk a megoldás kapujában. Az előbbi függvények összekapcsolásával
# adottak a zárak illetve a kulcsok. Mivel diszjunk halmazokról van szó,
# a Descartes szorzattal dolgozhatunk. Mikor fogadunk el egy zár-kulcs párt?
# Ha a két ötösben szereplő számok páronkénti összege nem lépi túl a hetet.
# Míg APL-ben könnyedén összeadunk két vektort/listát, sok programnyelvben ehhez
# használni kell a zip-et (mint a transzponáció Python-os kódjában). Itt viszont
# használhatjuk a map-et, majd egy újabb map-el külön-külön megvizsgálhatjuk az
# összeg minden tagját. Ha sehol nem léptük túl a határt, akkor ez a pár OK,
# növelhetjük a számlálót, ami végül megadja a megoldást.

(de part1 (File)
   (let (
      Groups (read-input File)
      Locks (locks Groups)
      Keys (keys Groups)
      Counter 0)

      (for L Locks
         (for K Keys
            (let (
               Sum (mapcar + L K)
               Result (apply and (mapcar '((X)(>= 7 X)) Sum)))
               (when Result (inc 'Counter)))))
      Counter))
