# A 11. feladatnál jön az exponenciális robbanás. Speciális szabályok alapján
# újabb és újabb számokkal kell dolgoznunk, és a végső kérdés az, hogy hány számunk van.
# Mivel csak pár számot tartalmaz az input, ezt bemásoltam magába a kódba.

(setq *My-puzzle (list 125 17))

# Ezután implementálni kellett a speciális függvényünket konkrét számokra.

(de blink (X)
   (cond
      ( (=0 X) (list 1))
      ( (=0 (% (length X) 2))
         (let (
            Xs (chop (format X))
            Half (/ (length X) 2)
            First (format (pack (head Half Xs)))
            Second (format (pack (tail Half Xs))))
            (list First Second)))
      (T (list (* 2024 X))) ))

# Majd ezt általánosítjuk számok listájára is.

(de blink-list (L)
   (make
      (for X L
         (chain (blink X)))))

# Ezt követi az általánosított függvény többszöri alkalmazása.

(de blink-list-sequence (N L)
   (setq *Sequence L)
   (do N
      (setq *Sequence (blink-list *Sequence)))
   (length *Sequence))

# És ezzel lényegében kész is vagyunk.

(de part1 ()
     (blink-list-sequence 25 *My-puzzle ))

# Part 2
# Az exponenciális robbanásra jellemző, hogy nyers erővel meg lehet oldani
# az első részét a feladatnak, de a másodikhoz már évszázadokig kellene futni
# a programnak. Ha az ember kicsit figyeli a számok viselkedését, elég hamar
# ismétlődésekre lel, s sose jó ugyanazt többször kiszámolni.

# Az rejtvényben szereplő számok helyett párokat használunk, ahol a pár első tagja az eredeti számot,
# a második tagja pedig a multiplicitását tartalmazza, ami kezdetben egy.
# Ezt az alábbi rutinnal állitjuk elő.

(de once (X) (cons X (1)))

# Ugyanazok a számok a lista több részén is felbukkanhatnak, ezért használunk egy rendezést,
# de az egymás után álló azonos számokat még össze kell vonni.

(de order (Number Count New Old)
   (cond
      ((= New NIL) (cons (list Number Count) Old))
      ((= Number (caar New)) (order Number (+ Count (cadar New)) (cdr New) Old))
      ((<> Number (caar New)) (order (caar New) (cadar New)  (cdr New) (cons (list Number Count) Old)))))

# A korábbi rutint ennyire kellett elbonyolítani, hogy kezelje és karbantartsa a párokat.

(de blink-list2 (Pairs)
   (let (
      Result
      (make
         (for Pair Pairs
            (chain
               (mapcar '((X) (cons X (cdr Pair))) (blink (car Pair)))
               )))
      Sorted (sort Result))
      (order (caar Sorted) (cadar Sorted) (cdr Sorted) NIL)))

# Ezután már nincs lényegi változás, maximum az indításkor kell a számokat párokká alakítani.

(de blink-list-sequence2 (N L)
   (setq *Sequence L)
   (do N
      (setq *Sequence (blink-list2 *Sequence)))
   (apply +(mapcar cadr *Sequence)) )


(de part2 (N)
     (blink-list-sequence2 N (mapcar once *My-puzzle) ))