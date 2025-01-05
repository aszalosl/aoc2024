# A 19. feladat az exponenciális robbanásról tanít.
# Az a kérdés, hogy egy bizonyos sorozat valamilyen építőkövekből felépíthető-e.
# Miután több mint 400 építőkövünk van, és a sorozat is elég hosszú, valamint
# mivel nem egyértelmű az összeállítás sem, a sima szélességi keresés
# biztos eltartana évekig is. Ezért érdemes mindenképpen bizonyos gyorsításokat
# bevezetni, még akkor is, ha ez elbonyolítja a programot.

# Az egyik ilyen, hogy miután számunkra az a fontos, hogy meddig jutottunk el
# a minta összeállításában, a Fringe sorba nem ész nélkül dobáljuk be a
# pozíciókat, hanem csak akkor, ha még korábban nem szerepelt (Known).
# A másik az, hogy az építőköveket nem egy sima listába gyűjtjük, hanem
# első három karaktere szerint listák listáját építjük fel, és hol az allisták
# feje jelzi, hogy érdemes-e azzal foglalkozni, vagy sem.

# A mintaillesztésre a "pre?" függvényt használjuk, s hogy ezt használhassuk,
# mind a soron következő sort, mint a mintákat/építőköveket karakter-listaként
# kezeljük.

(setq *Patterns NIL *Line NIL)

(de bfs (Fringe)
   (let Known NIL
      (loop
         (T (= NIL (car Fringe)) (prin "-") (not T))
         (T (< (length *Line) (car Fringe)) (prin "+") T)
         (let (
            Index (pop 'Fringe)
            Remaining (nth *Line Index) )
            (for PatternGroup *GPatterns
               (when
                  (pre? (car PatternGroup) Remaining)
                     (for P (cdr PatternGroup)
                        (when
                           (pre? P Remaining)
                           (let NewIndex (+ Index (length (car P)))
                              (when (not (member NewIndex Known))
                                 (queue 'Fringe NewIndex)
                                 (push 'Known NewIndex) ))))))))))

# Az első sor tartalmazza a mintákat - vesszővel és szóközzel elválasztva -
# ezért kicsit trükközni kell a beolvasással. Majd két lépcsőben kialakítjuk
# a kétszintű adatszerkezetet.
# Ezután a további sorokra alkalmazzuk az előbb látott szélességi keresést, ami
# logikai eredményt ad vissza. Ha igazat kapunk eredményül, növeljük a számlálót.

(de part1 (File)
   (let Counter 0
      (in File
         (setq *Patterns (split (filter '((X) (<> X " ")) (line)) ","))
         (setq *GPatterns
            (group (make (for P *Patterns (link (list (head 3 P) P))))))
         (until (eof)
            (setq *Line (line))
            (when
               (and
                  (< 0 (length *Line))
                  (bfs (list 1)) )
               (inc 'Counter))))
      Counter))

# Part 2
# Hosszabb ideig csavargattam az előbbi megoldást, hogy megoldja a feladat
# második részét. Aztán miután rendesen elbonyolítottam, kicsit távolabbról
# újra megvizsgáltam a feladatot, s rájöttem, hogy ez egy dinamikus programozási
# feladat, érdemes úgy is foglalkozni vele, s nem előről kell megküzdeni vele,
# hanem hátulról kell felgöngyölíteni.
# Van egy számlálólistánk, s hogy egyszerűbben növeljük egyes elemeit,
# bevezetünk egy segédfüggvényt.

(de inc-counter (L From To)
   (let (Lfrom (get L From) Lto (get L To))
      (put L To (+ Lfrom Lto]

# A számolás lényegi része itt történik, a számlálólistát nullázzuk, majd az
# utolsó betű után beszúrunk egy egyest (a semmit egyféleképpen ismerjük fel).
# Majd betűnként haladunk vissza (I ciklusa), s végigpróbáljuk az összes mintát.
# (Lehetne az előbb látott módon gyorsítani rajta, hogy kevesebb
# összehasonlításunk legyen, de csak elbonyolítja a kódot, érezhetően nem
# gyorsít most.) Ha illeszkedésre akadtunk, az aktuális kezdőponthoz
# hozzáadjuk a minta mögött szereplő számot - ami valójában a minta utáni
# szöveg cache-e. Végül az első karakternél található számot kell visszaadni,
# ami nem más, mint a lehetséges felbontások száma.

(de dp ()
   (let Counter (make (do (length *Line) (link 0)))
      (queue 'Counter 1)
      (for I (range (length *Line) 1)
         (let Remaining (nth *Line I)
            (for P *Patterns
               (when (pre? P Remaining)
                  (let NewI (+ I (length P))
                     (inc-counter Counter NewI I))))))
      (car Counter)))

(de part2 (File)
   (let Counter 0
      (in File
         (setq *Patterns (split (filter '((X) (<> X " ")) (line)) ","))
         (until (eof)
            (setq *Line (line))
            (when
               (< 0 (length *Line))
               (inc 'Counter (dp)))))
      Counter))