# Annak érdekében, hogy ne bonyolódjak bele az irányok kódjaiba, használjunk
# pár egyértelmű konstansts!

(setq Up 0 Right 1 Down 2 Left 3)

# Olvassuk be az inputot, ami már megint egy térkép, illetve állítsuk be
# a cél pozícióját!

(de load-data (File)
   (in File
      (setq *Table (make (until (eof) (link (line))))
            EX (- (length (car *Table)) 1) EY 2)))

# Létrehozunk egy a térképpel megegyező mátrixot, amelybe azt írjuk be,
# hogy az adott pozíció milyen költséggel érhető el.
# Illetve ha már itt vagyunk akkor egy sort is, melyben a feldolgozandó
# pozíciók szerepelnek.

(de generate-empty ()
   (setq
      *Fringe (list (list 0 2 (- (length (car *Table)) 1) Right))
      *Values
         [make (for Line *Table (link (make (for C Line (link NIL]
      ))

(de draw (Map)
   (for Line Map
      (prinl (pack Line)))
   (prinl) )

# A későbbiekben minden iránnyal próbálkozunk, így az alábbi függvényben
# teszteljük, hogy sikeres-e a próbálkozás.
# Megnézzük, hogy a megcélkozott pozíció járható - azaz ott nincs fal -,
# illetve még nem jártunk ott, vagy csak drágább úton haladva értük el.
# Ha ezek teljesülnek, akkor a pozícióhoz feljegyezzük a hozott költséget,
# illetve a feldolgozandó pozíciók végére feljegyezzük ezt is.

(de forward (X Y Value Dir)
   (when
      (and
         (or (= "." (get *Table Y X)) (= "E" (get *Table Y X)))
         (or (< Value (get *Values Y X)) (= NIL (get *Values Y X))))
      (put *Values Y X Value)
      (queue '*Fringe (list Value X Y Dir))
            ))

# Lényegében egy szélességi keresést hajtunk végre. Ehhez tekintjük a "Fringe"
# sorunk  elején szereplő négyest, mely az aktuális pozíciót,
# a hozzá tartozó költséget és irányt tartalmazza.
# A rövidebb kód érdekében még bevezetünk pár további változót.
# Előbb azt vizsgáljuk meg, hogy az aktuális irányt tudjuk-e
# folytatni. Majd azt, hogy van-e lehetőség kanyarodásra.
# Ha valamely irány szabad, akkor ott újabb csúcsot szúrunk be a sorba.
# Mivel itt minden ciklusmag a sor fejének törlését jelenti, érdemes
# figyelni, hogy kiürült-e a sor, azaz felderítettük-e a teljes térképet.
# Ha igen, akkor lilépünk.

(de bfs ()
   (loop
      (let (
         First (pop '*Fringe)
         Val (car First)
         X (cadr First)
         Y (caddr First)
         Dir (cadddr First)
         Ym (dec Y) Yp (inc Y) Xm (dec X) Xp (inc X) Vp (inc Val) Vpp (+ 1001 Val))
         # (when (and (= X EX) (= Y EY)) (print Val) (quit))
         (when (= Up Dir) (forward X Ym Vp Up))
         (when (= Down Dir) (forward X Yp Vp Down))
         (when (= Right Dir) (forward Xp Y Vp Right))
         (when (= Left Dir) (forward Xm Y Vp Left))
         (when (or (= Dir Up) (= Dir Down))
            (forward Xp Y Vpp Right)
            (forward Xm Y Vpp Left)
            )
         (when (or (= Dir Left) (= Dir Right))
            (forward X Yp Vpp Down)
            (forward X Ym Vpp Up)
            ))
      (T (= NIL *Fringe)
         (print (get *Values 2 (dec (length (car *Table)))))
         ) ))

# A kezdeti beállítások részeként a kezdőpozícióba elhelyezzük a 0 értéket,
# majd elindítjuk a szélességi keresést.

(de part1 (File)
   (load-data File)
   (generate-empty)
   (put *Values (- (length *Values) 1) 2 0)
   (bfs)
   )

# Part2
# A feladat második felében az a kérdés, hogy hány mező tartozik optimális
# úthoz. Miután az első esetben már kiszámoltuk, hogy egy mezőhöz milyen
# költség tartozik, érdemes erre alapozni.
# Visszafele fogunk haladni, és azt nézzük, hogy mely szomszédos mezőről
# juthatunk ide. Mivel egy mezőhöz csak egy értéket rendeltünk, pedig
# fordulás esetén akár két értéket is rendelhetnénk, a tárolt értékünkre
# felső korlátként fogunk tekinteni és az ezres maradékot figyeljük.
# Ha illeszkedő lépést találtunk, akkor azt hozzáadjuk a sorhoz.
# Minden meglátogatott mezőt bejelölünk egy globális mátrix változóban.

(de backward (X Y Value Dir)
   (when
      (and
         (or (= "." (get *Table Y X)) (= "S" (get *Table Y X)))
         (= (% Value 1000) (% (get *Values Y X) 1000))
         (<= (get *Values Y X) Value))
      (put *Map Y X "O")
      (queue '*Fringe (list Value X Y Dir)) ))

# Az első részhez hasonlóan haladunk most is, egy sort használunk, rendre
# annak fejét dolgozzuk fel. Irányban haladva eggyel csökkentjük a határt,
# fordulás esetén pedig 1001-el. Ha kiürül a sor, akkor kész vagyunk.

(de bfs2 ()
   (loop
      (let (
         First (pop '*Fringe)
         Val (car First)
         X (cadr First)
         Y (caddr First)
         Dir (cadddr First)
         Ym (dec Y) Yp (inc Y) Xm (dec X) Xp (inc X) Vm (dec Val) Vmm (- Val 1001))
         # (println "Node: " First)
         (when (= Up Dir) (backward X Ym Vm Up))
         (when (= Down Dir) (backward X Yp Vm Down))
         (when (= Right Dir) (backward Xp Y Vm Right))
         (when (= Left Dir) (backward Xm Y Vm Left))

         (when (= Up Dir) (backward X Ym Vmm Left))
         (when (= Down Dir) (backward X Yp Vmm Left))
         (when (= Right Dir) (backward Xp Y Vmm Up))
         (when (= Left Dir) (backward Xm Y Vmm Up))

         (when (= Up Dir) (backward X Ym Vmm Right))
         (when (= Down Dir) (backward X Yp Vmm Right))
         (when (= Right Dir) (backward Xp Y Vmm Down))
         (when (= Left Dir) (backward Xm Y Vmm Down)))
      (T (= NIL *Fringe)
         (print (length (filter '((X) (= "O" X)) (apply append *Map))))
         ) ))

# Be kell állítani a globális változóinkat, majd indíthatjuk a szélességi
# keresést.

(de part2 ()
   (setq
      *Fringe (list (list
         (get *Values 2 (- (length (car *Table)) 1))  # Value
         (- (length *Table) 1) 2  # position
         Down))  # dir
      *Map (make (for Line *Table (link (make (for C Line (link ".")))))))
   (put *Map 2 (- (length (car *Table)) 1) "O")
   (bfs2) )