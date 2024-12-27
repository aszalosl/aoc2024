# Kicsit kellemetlen az adatok beolvasása, mert egy üres sorral elválasztva két fajta
# adatunk is van. Az első részben "bar"-ral elválasztott két szám szerepel egy sorban,
# míg a második részben vesszővel elválasztott számok (immár sok) szerepelnek a sorokban.
# Mindkét esetben a split segít rajunk, mellyel ez alapján felszabdalhatjuk a sorainkat.
(de load_data (F)
   (let L ""
      (in F
         (setq *Orders (make
               (until (= (setq L (line)) "")
                  (link (mapcar pack (split L "|"))))))
         (println L)
         (setq *Updates (make
               (until (eof)
                  (link (mapcar pack (split (line) ","))))))) ))
# Az első részben szereplő párok megadnak egy matematikai relációt - amit nem érdemes
# totális rendezésse alakítani. Mi azt számoljuk össze, hogy egy adott szám 
# hány másik elemmel áll relációban.
(de collect_firsts (N)
   (mapcar car (filter '((X)(= (cadr X) N)) *Orders)))

# El szeretnénk dönteni, hogy adott számsorozat (egy sor az input második feléből) # megfelelő sorrendben van-e. Ehhez sorbavesszük a sorozat elemeit, mindre külön-külön
# teszteljük, hogy a szabályok szerint mely számok között kellene állniuk, és most
# milyen számok követik a sorozatban. Ha e két halmaz metszete (sect) nem üres, 
# akkor már biztos nincsenek jó sorrendben.. 
(de correct_order (L)
   (if (cadr L)
      (let Ps (collect_firsts (car L))
         (if (sect Ps (cdr L))
            NIL
            (correct_order (cdr L))))
      T))
# A feladat szövegezése szerint a sorozat középső elemére vagyunk kíváncsiak, 
# itt az "nth" függvényt használhatjuk. 
(de middle (L)
   (let Len (length L)
      (format (car (nth L (+ 1 (/ Len 2)))))))

# Miután beolvastuk az inputot, meg kell szűrni az abban szereplő sorozatokat,
# a megmaradottakból kiválogatni a középső elemeket, majd ezeket összeadni
(de part1 (F)
   (load_data F)
   (apply + (mapcar middle (filter correct_order *Updates))))

# part 2 ----------------

# A feladat második felében a szabályok szerint helytelen sorozatokat kell rendberakni.
# Mivel a totális rendezés nem elérhető, a rendezése soronként kell megtennünk.
# Ehhez megszámoljuk hogy a sorozatban szereplő elemeket hány előzheti a sorozat 
# többi eleme közül. Ezt az elemszámot tekintjük a rendezés alapjának, és kész is vagyunk.
(de foregoing (X L)
   (let F (collect_firsts X)
      (sect F L)))

(de middle_ordered (L)
   (middle
      (mapcar cadr
         (sort
            (make
               (for X L (link (list (length (foregoing X L)) X))))))))

# Az elv ugyanaz mint az első esetben, csupán apróbb eltérések vannak.
(de part2 ()
   (apply + (mapcar middle_ordered
         (filter '((X)(not (correct_order X))) *Updates))) )
