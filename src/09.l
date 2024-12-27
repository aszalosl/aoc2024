# A kilencedik feladat a háttértár töredezésmentesítéséről szól. Mivel a nyelv továbbára
# sem rendelkezik tömbökkel, a legnyilvánvalóbb implementációt elfelejthetjük. A Rosetta Code
# oldalán találkozhatunk duplán láncolt lista Picolisp implementációjával, azt igénybe is
# vehetnénk a tömb/array helyett, de talán két megfelelően megválasztott listával is célhoz érünk.
# Most az input egyetlen - de igen hosszú sor, ahol a számjegyek a foglalt és üres helyek méretét
# adja meg. A keresett adat egy checksum, amit a folyamat végi állapotból lehet meghatározni.
# Viszont mivel az üres helyek feltöltésével balról jobbra haladunk, az itt tárolandó adaktól
# megszabadulhatunk, és csak a feldolgozásra váró adatokat fogjuk tárolni.

(setq *Empty -1)

(de load-data (File)
   (in File
      (let Line (mapcar format (line))
         (make
            (for (I . Number) Line
               (if (=1 (% I 2))
                  (link (list (/ (- I 1) 2)  Number))
                  (when (< 0 Number) (link (list *Empty  Number))) ))))))

# Az első megoldás natívan rekurzív volt, de nem nem használta a Picolisp lehetőségeit (https://picolisp.com/wiki/?recurInPicoLisp).
# S mivel igen csak hosszú string a feladat inputja, természetesen jött a stack overflow.
# Emiatt alább már egy iteratív - ám működő - megoldás szerepel, ahol bevezetünk pár lokális és pár globális változót, melyek értékeit
# az adott feltételeknek megfelelően módosítjuk.

(de steps ()
   (setq IDF (caar *Forward) IDB (caar *Backward) LenF (cadar *Forward) LenB (cadar *Backward))
   (until (or (> IDF IDB) (and (= IDF IDB) (= 0 LenB)))
      (cond
         # final steps
         ( (and (= IDF IDB) (< 0 LenB))
            (setq *Backward (cons (list IDB (dec LenB)) (cdr *Backward)))
            (inc '*Checksum (* IDB *Position))
            (inc *Position))
         # occupied positions
         ( (and (<= 0 IDF) (< 0 LenF))
            (setq *Forward (cons (list IDF (dec LenF)) (cdr *Forward)))
            (inc '*Checksum (* IDF *Position))
            (inc '*Position) )
         ( (and (<= 0 IDF) (=0 LenF))
            (setq *Forward (cdr *Forward)) )
         # empty positions
         ( (and (= *Empty IDF) (< 0 LenB) (< 0 LenF))
            (setq
               *Forward (cons (list IDF (dec LenF)) (cdr *Forward))
               *Backward (cons (list IDB (dec LenB)) (cdr *Backward)) )
            (inc '*Checksum (* IDB *Position))
            (inc '*Position) )
         ( (and (= *Empty IDF) (= 0 LenB))
            (setq *Backward (cdr *Backward)) )
         ( (and (= *Empty IDF) (= 0 LenF))
            (setq *Forward (cdr *Forward)) ))
      (setq IDF (caar *Forward) IDB (caar *Backward) LenF (cadar *Forward) LenB (cadar *Backward)) )
   *Checksum )

# Nem bonyolítjuk a helyzetet, az áthelyezendő egységeket leíró listából töröljük az üres szakaszokat, mert azokat nem rakosgatjuk sehova.

(de remove-empty (XY)
   (<> *Empty (car XY)))

(de part1 (File)
   (let *Forward (load-data File)
      (setq *Backward (filter remove-empty (reverse *Forward)) *Position 0 *Checksum 0)
      (println (steps)) ))

# Part 2
# A második részben nem blokkokat mozgatunk, hanem egész egységeket. Szintén az első - elegendő nagy - helyre kerül be a legutolsó megfelelően
# kicsi egység. Ha nem tudjuk a hátsó egységekkel betömni a lukakat, akkor azok a helyükön maradnak. Emiatt most szétszedhetjük mozgatást és az
# ellenőrző összeg számítását.

# Először is nézzük meg, hogy hol van az az első üres hely, ahol egy adott méretű fájlt beszúrhatunk.
# Sajnos kicsit körülményes egy listán ciklussal végighaladni, hogy közben bármikor kiugorhassunk a ciklusból.
# A https://picolisp.com/wiki/?deeperLookAtFor segít tisztázni a dolgokat.

(de fit (Size Fwd)
   (for (I . Pair) Fwd (T (and (= (car Pair) *Empty) (<= Size (cadr Pair))) (list I (cadr Pair)))))

# A Picolisp dokumentációja igen alaposan tárgyalja, hogy mely utasítások nem zavarják össze a listáinkat (non-destructive).
# Viszont ha ezeket meg kívánjuk erőszakolni, hogy mégis megváltoztassuk a listákat, azaz itt-ott lecseréljünk benne valamit,
# akkor a lábon lőjük magunkat. Nekem is órákat vett el az, hogy a place utasítást erőltettem a "hagyományos" put/get helyett.
# Mindezek eredménye gigabájtos log fájlok, fura szerkezetű listák lettek, amit piszok nehéz volt debuggolni.
#
# Egyébként hasonlóan dolgozunk, mint az előbb, visszafele haladunk a mozgatható fájlok listáján (Backward,
# de mivel meg lett fordítva, valójában a normális irányt követjük).
# Megnézzük, hogy van-e olyan luk, ahova ez belefér, illetve ez előtte, vagy utána található.
# Természetesen csak előre mozgathatjuk. Két esetet kell megkülönböztetni, 1) ha teljesen elfoglalja a rendelkezésre álló helyet,
# ekkor a lista két eleme helyet cserél. 2) Ha nagyobb helyre rakjuk be, mert ekkor a helyén egy luk lekeletkezik, az eredeti luk
# kicsit összehúzza magát, és a fájlt be kell szúrni (insert) az eredetileg üres rész elejére.

(de steps2 (Fwd Backward)
   (let Forward Fwd
      (for Pair Backward
         (let (IDB (car Pair) Size (cadr Pair) Result (fit Size Forward) I (car Result) EmptySize (cadr Result))
            (when Result
               (let IB (index Pair Forward)
                  (prinl IDB "/" Size "   index:" I "  emptysize: " EmptySize "   old pos: " IB )
                  (when (< I IB)
                     (when (= Size EmptySize)
                        (put Forward IB (list *Empty Size))  # replace the file with empty
                        (put Forward I Pair) # move the file into the empty space
                        (when (< Size EmptySize)
                           (put Forward IB (list *Empty Size)) # replace the old with empty
                           (put Forward I (list *Empty (- EmptySize Size))) # decrease the empty place
                           (set 'Forward (insert I Forward Pair))) ))))))
      Forward))

# Mivel az ellenőrző összeg feldolgozását el kellett választani a mozgatástól, így külön függvényt kapott.
# A korábbiakból tanulva a rekurzió helyett itt is inkább ciklust használunk.

(de calculate-checksum (Forward)
   (let (Position 0 Checksum 0)
      (for Pair Forward
         (let (ID (car Pair) Size (cadr Pair))
            (if (= *Empty ID)
               (inc 'Position Size)
               (for I Size
                  (inc 'Checksum (* Position ID))
                  (inc 'Position) ))))
      Checksum))

(de part2 (File)
   (let *Forward (load-data File)
      (let Result (steps2 *Forward (filter remove-empty (reverse *Forward)))
         (println (calculate-checksum Result)))))
