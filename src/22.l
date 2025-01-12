# A huszonkettes feladatban egy egyszerű algoritmus található. A benne szereplő
# alkotóelemeket ismeri a nyelv, és nagy számokkal is képes dolgozni, így
# ezt könnyű implementálni. Az sem túl nehéz, hogy mindezt egy ciklusba rakjuk,
# ami ebben az esetben nem a már jól ismert for, hanem az előírt lépésszámú do.

(de step (SecretNumber)
    (let (
        S1 (% (x| (* 64 SecretNumber) SecretNumber) *REM)
        S2 (% (x| (/ S1 32) S1) *REM)
        S3 (% (x| (* 2048 S2) S2) *REM))
        S3))

(de two-thousandth (X)
    (let Y X
        (do 2000
            (setq Y (step Y)))
        Y))

# A feladat első feléhez nem marad más, mint sorra beolvasni az input sorait,
# s arra alkalmazni az előbbi függvényt. Azok eredményeit összegezni kell - ám
# jól tesszük ha utolsó utáni sorhoz tartozó NIL-től megszabadulunk.

(de part1 (File)
    (in File
        (apply +
            (cdr
                (reverse
                    (make
                        (until (eof)
                            (link (two-thousandth (read]

# Part 2

Majd 1800 sorból áll az input, s mindegyiknél 2000 lépést kell megtenni.
Ez már szép szám. A kérdés az, hogy adott négyjegyű kód melyik sorozatnál
hol fordul elő. Ha még ehhez egy harmadik ciklust is rendelnénk, akkor
várhatnánk a végeredményre. Tehát valamilyen másik utat kell keresni. Jó lenne,
ha egy-egy számmal az inputból egyszerre tudnánk végezni. Épp ezért ahogy annak
2000 utódján végigrohanunk, kiszámoljuk a belőle származó kódot, illetve annak
hatását.
Bár az volt az elképzelésem, hogy a kódot számként kezelem, de valamiért a
maradékok különbsége és a maradékosztályok különbsége (illetve ennek maradéka)
nem ugyanaz, s még most sem tudom miért, egy listában tároljuk a kód részeit.
Hogy elindulhassunk, s kiszámoljuk az első kódot, kell tennünk pár lépést, és
számolnunk sok maradékot.


(de starting-code (SecretNumber)
  (let (
     S1 (step SecretNumber) S2 (step S1) S3 (step S2) S4 (step S3)
     R0 (% SecretNumber 10) R1 (% S1 10) R2 (% S2 10) R3 (% S3 10) R4 (% S4 10)
     Code (list (- R1 R0) (- R2 R1) (- R3 R2) (- R4 R3)))
     (list Code S4)))

Mivel a lehetséges kódok száma valahol tízezer és százharminc ezer között van,
listában tárolni ezeket nem a legszerencsésebb. Tömböket nem ismert a nyelv,
szerencsére viszont van bináris fánk. Egy-egy csúcsába egy pár fogunk
elhelyezni, amely a kódból, és az általa nyert banánok számából áll.
Természetesen kezdetben ez a fa üres:

(setq *Counters NIL)

# Lássuk, hogyan szánkázunk végig az összes lehetséges áron!
# Itt is "do" ciklust használjuk, és lesz egy lokális listánk,
# mely az itt már előfordult kódokat tárolja. Ha valamelyik újra feltűnik,
# azzal már nem szerzünk semmit, mert mindig csak az első előfordulás számít.
# Valószínűleg itt is hatékonyabb lenne egy bináris fát használni, bár kétezernél
# több kód nem fordulhat elő.
# Ha új kóddal találkozunk, azt feljegyezzük, majd a kódhoz tartozó pár
# megpróbáljuk előhalászni a fából. Csupán a pár első felét, a kódot ismerjük,
# de a kereséshez ez is elég lesz. Az, hogy a kód ennek az árusnak új, másnál már
# előfordulhatott, így bekerülhetett a fába.
# Ha még egyáltalán nem fordult elő, akkor most berakjuk az előírt szabálynak
# megfelelő banánmennyiséggel. Ha viszont már szerepelt, akkor a párt kinyerve,
# biztonsági játékosként ki is töröljük a fából, nehogy azt vegyük elő egy újabb
# pár helyett. Ezután a szabálynak megfelelő módon megnöveljük a banánok számát,
# és visszaírjuk a párt a fába.
# Ha mindez megtörtént, lépünk egyet, kiszámoljuk az új titkos számot, az új
# kódot, illetve frissítjük a legutóbbi maradékot, mert arra szükségünk lesz még.

(de sequence (StartCode SecretNumber)
   (let (
      C StartCode
      SN SecretNumber
      Rlast (% SecretNumber 10)
      Occured NIL)
      (do 1997
         (when (not (member C Occured))
            (push 'Occured C)
            (let ValuePair (lup *Counters C)
               (if (not ValuePair)
                  (idx '*Counters (cons C (% SN 10)) T) # insert the new
                  (idx '*Counters ValuePair NIL)  # delete the old
                  (idx '*Counters (cons C (+ (cdr ValuePair) (% SN 10))) T))))  # update the value
        (setq
           SN (step SN)
           C (append (tail -1 C) (list (- (% SN 10) Rlast)))
           Rlast (% SN 10)))))

# Ezután nincs más dolgunk, mint ez a függvényt elindítani az inputban szereplő
# összes számra - miután kiszámoltuk a hozzá tartozó kezdőértékeket. Majd a fában
# szereplő összes szám közül kell venni a legnagyobbat.
# (A kód már senkit nem érdekel.)

(de part2 (File)
      (in File
         (until (eof)
            (let SN (read)
               (when SN
                  (setq SC (starting-code SN))
                   (sequence (car SC) (cadr SC))))))
   (apply max (mapcar cdr (idx '*Counters))))


