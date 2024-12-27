# Az inputot megkavarja kicsit az első szám mögött álló kettőspont.
# Ezért ezt első lépésben kitöröljük az adott sorból, majd a szóközök
# mentén szétszabdaljuk a sort, a darabokból számokat csinálunk. 
# Egy sorhoz tartozó számokból listát készítünk, illetve miután ezt megismételjük
# minden egyes sorra, listák listájával kell a továbbiakban dolgozni.

(de read-input (I)
   (make
      (in I
         (until (eof)
            (link
               (mapcar
                  '((X)  (format (pack X)))
                  (split
                     (filter '((X) (not (= X ":"))) (line))
                     " ") ))))))

# Azt kell ellenőrizni, hogy a megadott számokból összeadás és szorzás segítségével
# elő tudjuk-e állítani a megadott számot. Mi pont a másik irányba haladunk, azaz
# azt nézzük meg, hogy le tudjuk-e bontani a számot. Ehhez pedig visszafele indulunk el
# a megadott számlistán - éppen ezért megfordítottuk.
# Ha a számlistánkban már csak egy szám van, és az a keresett, akkor kész vagyunk, 
# a szám előállítható.	
# Akár kifogyott a lista, akár negatív számot sikerült kivonással generálni, ez a irány téves.
# Ha az aktuális szám nem osztható a lista első tagjával, akkor csak összeadás lehetett 
# a megfelelő művelet, tehát ezt a számot ki kell vonnunk.
# Egyébként pedig két lehetőség is áll előttünk, mindkettőt ki kell próbálni, azaz osztanunk is kell, 
# és kivonnunk is. Egyedül a lusta kiértékelésben bízhatunk, hogy nem számolunk feleslegesen.
# Az összes lehetőség felsorolását a rekurzió megoldja számunkra.

(de check (N Ns)
   (cond
      ( (and (= N (car Ns)) (= (cdr Ns) NIL)) T) # base case
      ( (< N 0) NIL)
      ( (= Ns NIL) NIL) # no hope
      ( (and
            (<> 0 (% N (car Ns)))
            (> N (car Ns)) )
         (check (- N (car Ns)) (cdr Ns)))
      ( T (or
            (check (/ N (car Ns)) (cdr Ns))
            (check (- N (car Ns)) (cdr Ns))))))

(de test (Line)
   (let (N (car Line) Ns (reverse (cdr Line)))
      (if (check N Ns) N 0)))

(de part1 (File)
   (let Numbers (read-input File)
      (prinl (apply + (mapcar test Numbers)))))

# Part 2

# A második részben megjelent egy új művelet, a konkatenáció. Természetesen ha
# visszafele haladunk, ennek a csonkolás felel meg, persze ha az van a szám végén,
# amit le szeretnénk onnan vágni. Erre használjuk a "slice" függvényt. Ha csonkolható,
# akkor a csonk lesz az eredmény, különben a hamis.
 
(de prefix (Prefix Full)
   (cond
      ((= Prefix NIL) Full)
      ((= Full NIL) NIL)
      ((<> (car Prefix) (car Full)) NIL)
      ((= (car Prefix) (car Full)) (prefix (cdr Prefix) (cdr Full))) ))
  
(de slice (Long Short)
   (let (RSLong (reverse (chop (format Long))) RSShort (reverse (chop (format Short))))
      (let P (prefix RSShort RSLong)
         (if P (format (pack (reverse P))) NIL))))

(de check2 (N Ns)
   (if
      (or (not Ns) (= NIL (car Ns)) (< N 0))
      NIL
      (let (Remainder (% N (car Ns)) Slice (slice N (car Ns)) )
         (cond
            ( (and (= N (car Ns)) (= (cdr Ns) NIL)) T) # base case
            ( (and (<> N (car Ns)) (= (cdr Ns) NIL)) NIL) # no hope
            ( (and
                  (<> 0 Remainder)
                  (not Slice)
                  (> N (car Ns)) )  (check2 (- N (car Ns)) (cdr Ns)))
            ( (and # divisible
                  (not Slice)
                  (> N (car Ns)) )
               (or
                  (check2 (/ N (car Ns)) (cdr Ns))
                  (check2 (- N (car Ns)) (cdr Ns))))
            ( (and # splittable
                  (<> 0 Remainder)
                  (> N (car Ns)) )
               (or
                  (check2 Slice (cdr Ns))
                  (check2 (- N (car Ns)) (cdr Ns))))
            ( T (or
                  (check2 Slice (cdr Ns))
                  check2 (/ N (car Ns)) (cdr Ns))
               (check2 (- N (car Ns)) (cdr Ns))))))))

# A többi lényegében már ugyanaz mint az előbb, csak az újabb verziójú függvényeket kell alkalmazni.
(de test2 (Line)
   (let (N (car Line) Ns (reverse (cdr Line)))
      (if (check2 N Ns) N 0)))


(de part2 (File)
   (let Numbers (read-input File)
      (prinl (apply + (mapcar test2 Numbers)))))
