# Egy újabb térkép-bejárási feladat következik. Most térképen szereplő akadályok
# koordinátáit tartalmazza a input. Több módon is beolvashatnánk ezeket,
# használjuk a 14. feladatnál már szereplő módszert!

(de to-number (X) (format (pack X]

(de load-data (File)
   (in File
      (make
         (until (eof)
            (let Line (line)
               (use (@A @B)
                  (match '(@A "," @B) Line)
                  (link (list (to-number @A) (to-number @B]

# Használunk pár konstanst, mint például a térkép méreteit. Mivel a PicoLisp
# esetén a listák indexelés egytől történik, a feladatban pedig nullától,
# itt eggyel nagyobb számok szerepelnek. Itt állítunk elő egy üres térképet is.

(setq
   #*Size 7
   *Size 71
   *Corrupted -1
   *Empty -2
   *Table (make
      (for I *Size
         (link
            (make
               (for J *Size
                  (link *Empty]

# Az input megadott részét fel kell helyezni a térképre.

(de add-dropped (Ps)
   (for P Ps
      (put *Table (inc (cadr P)) (inc (car P)) *Corrupted]

# A változatosság kedvéért következzen egy szélességi keresés! Mivel a térkép
# fogja tartalmazni a meglátogatott csúcsokat, itt csak a határ pontjait kell
# nyilvántartani. Mivel ezeket sorként kezeljük, szélességi keresést kapunk.

(de bfs (Fringe)
   (let Result 0
      (loop
         (T (= NIL (car Fringe)) (setq Result (get *Table *Size *Size)))
         # (println Fringe)
         (let (
            Node (pop 'Fringe)
            X (car Node)
            Y (cadr Node)
            V (get *Table Y X)
            Xm (dec X) Xp (inc X) Ym (dec Y) Yp (inc Y) Vp (inc V))
            # (prinl "X: " X)
            # (prinl "Y: " Y)
            # (prinl "Vp: " Vp)

            (when (= *Empty (get *Table Y Xm)) (put *Table Y Xm Vp) (queue 'Fringe (list Xm Y)))
            (when (= *Empty (get *Table Y Xp)) (put *Table Y Xp Vp) (queue 'Fringe (list Xp Y)))
            (when (= *Empty (get *Table Ym X)) (put *Table Ym X Vp) (queue 'Fringe (list X Ym)))
            (when (= *Empty (get *Table Yp X)) (put *Table Yp X Vp) (queue 'Fringe (list X Yp)))))
      Result))

# Az indításhoz fel kell rakni a megadott számú akadályt, majd beállítani a
# keresős kezdőpontját, és indíthatjuk a keresést.

(de part1 (File Count)
   (let Drops (load-data File)
      (add-dropped (head Count Drops))
      (put *Table 1 1 0)
      (bfs (list (list 1 1]

# Part 2
# A feladat második felében lényegében az a kérdés, hogy hanyadik akadály
# fogja elzárni az utat. Lusta megoldásként egyre újabb és újabb akadályokat
# rakunk be, és nézzük, hogy sikeres-e a keresés. Persze egy bináris keresés
# sokat lendíthetne a futás sebességén, de most arra egyáltalán nem
# optimalizálunk. (Így is lefut a megoldás 5 másodperc alatt.)
# Viszont mivel újra és újra elindítunk egy keresést, a térképes újra és újra
# le kell takarítani.

(de make-empty ()
   (setq *Table (make
      (for I *Size
         (link
            (make
               (for J *Size
                  (link *Empty]

# Lényegében csak egy végtelen ciklust fűztünk a korábbi megoldás köré.

(de part2 (File Count)
   (let (
      Drops (load-data File)
      Counter Count
      Result NIL)
      (loop
         (make-empty)
         (add-dropped (head Counter Drops))
         (put *Table 1 1 0)
         (setq Result (bfs (list (list 1 1))))
         (T (= *Empty Result) (print (get Drops Counter)))
         (T (> Counter (length Drops)) (print "No sol."))
         (inc 'Counter))))