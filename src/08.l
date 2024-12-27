# A nyolcadik feladatban közös frekvenciájú antennákhoz tartozó pozíciókat kell meghatározni és összeszámolni.
# Az input egy térképet takar, így ezt listák listájaként olvassuk be.

(de read-input (File)
   (make
      (in File
         (until (eof)
            (link (line)) ))))

# Ezt követően meghatározzuk minden egyes antennának a pozíciót - ehhez a listák listáját dupla ciklussal bejárjuk -,
# s eltároljuk a frekvencia kódját, és a koordinátákat. Majd a "group" parancs csoportosítja frekvenciánként az antennákat,
# így újfent listák listáját kapjuk meg, ahol az allistákban a fej a frekvencia kódja, míg a farkot a koordináták párjai alkotják.

(de make-groups (File)
   (let Matrix (read-input File)
      (setq *BoardY (length Matrix) *BoardX (length (car Matrix)))
      (group
         (make
            (for (Y . Line) Matrix
               (for (X . Char) Line
                  (when (<> Char ".") (link (list Char X Y)))))))))

# Az azonos csoportba tartozó antennák közül párokat kell kiválasztani. Erre már van egy mintaprogram,
# mely általános kombinációkat készít (https://picolisp.com/wiki/?99p26), a teljesség kedvéért bemásolom ide.

(de combination (N Lst)
   (cond
      ((=0 N) '(NIL))
      ((not Lst))
      (T
        (conc (mapcar
            '((X) (cons (car Lst) X))
            (combination (dec N) (cdr Lst)) )
            (combination N (cdr Lst)) ))))

# Ha adott a két pár koordináta, akkor az egyiket a másikra tükrözve megkapjuk az egyik keresett pozíciót,
# s a másikat az egyikre tükrözve pedig a másikat.

(de antinodes (Pair)
   (let (X1 (caar Pair) Y1 (cadar Pair) X2 (caadr Pair) Y2 (cadadr Pair) DX (- X1 X2) DY (- Y1 Y2))
      (list
         (list (+ DX X1) (+ DY Y1))
         (list (- X2 DX) (- Y2 DY)))))

# Ami nem fér el a térképen, az nem számít. Tehát készítünk egy segédfüggvényt, amellyel szűrni lehet
# a megkapott pozíciókat.

(de on-the-table (XY)
   (and (<= 1 (car XY) *BoardX) (<= 1 (cadr XY) *BoardY)))

# Nincs más dolgunk, mint összekapcsolni az eddigi függvényeket. Az inputból kinyert csoportokból másodrendű
# kombinációkat képzünk, ezek lesznek a koordinátapárjaink.
# Majd ezen párok alapján meghatározzuk az újabb pozíciókat. Mindkét esetben a "make/chain" párost használjuk,
# hogy az eredmények ne tűnjenek el egymásba ágyazott listák mélyén.
# Végül nincs más dolgunk, mint tekinteni a térképen szereplő új pozíciókat - a multiplicitást a "uniq" megszünteti -,
# s mivel csak a darabszám a lényeges, így azt adjuk vissza.

(de part1 (File)
   (let Pairs
      (make
         (for Group (make-groups File)
            (chain (combination 2 (cdr Group))) ))
      (let Antinodes (make (for Pair Pairs (chain (antinodes Pair))))
         (length (uniq (filter on-the-table Antinodes))) )))

# Part 2
# A feladat második felében a keresett pozíciók definíciója változott meg, ennek megfelelően kell nekünk is
# változtani a megfelelő függvényen.
# Elindulunk az egyik pozíciótól és a különbségvektorral lépdelünk mindaddig, amíg a táblán maradunk.
# Majd ezt megismételjük a másik pozícióra is. S mivel itt már szűrünk a térképre, a "part2" függvényben
# erre már nem lesz szükség.


(de antinodes2 (Pair)
   (let (X1 (caar Pair) Y1 (cadar Pair) X2 (caadr Pair) Y2 (cadadr Pair) DX (- X1 X2) DY (- Y1 Y2))
      (make
         (while  (on-the-table (list X1 Y1))
            (link (list X1 Y1))
            (inc 'X1 DX)
            (inc 'Y1 DY))
         (while  (on-the-table (list X2 Y2))
            (link (list X2 Y2))
            (dec 'X2 DX)
            (dec 'Y2 DY)))))

(de part2 (File)
   (let Pairs
      (make
         (for Group (make-groups File)
            (chain (combination 2 (cdr Group))) ))
      (let Antinodes (make (for Pair Pairs (chain (antinodes2 Pair))))
         (length (uniq Antinodes)) )))
