# A 12. feladatban összefüggő tartományokat kell felfedezni az inputként szolgáló
# térképen. Ennek megfelelően az input mátrixából számunkra egy listák listája
# képződik, melyet globális változóként fogunk használni.

(de read-input (File)
   (setq *Table
      (make
         (in File
            (until (eof)
               (link (line)) )))))

# Habár a Picolisp szimulációs könyvtárában https://picolisp-explored.com/picolisp-explored-discrete-event-simulation
# ott lenne a Flood algoritmus, én megmaradok a hagyományos szélességi
# keresésnél. Ez annyival lett kibővítve, hogy mivel a térkép minden pozícióját
# fel kell dolgozni.
# Addig ismételjük a szélességi keresést, amíg ki nem ürítjük az Unvisited listát.
# A Fringe a feldolgozásra váró, már a látókörünkbe került csúcsokat tartalmazza,
# a Cluster pedig már a feldolgozattakat, azaz amelyeknek a négy szomszédját
# felemlegettük.
# Egy ilyen szomszéd akkor kerül be a Fringe-be, ha még nem szerepel ott, még
# nem lett feldolgozva, valamint ugyanabba a régióba tartozik (ugyanazon karakter
# jelöli). Mivel a "get" a térképen túl NIL-t ad vissza, ez már más régiót jelent,
# tehát itt nem kell aggodalmaskodni, hogy lemászunk a térképről.

(de bfs (Nodes)
   (let (Clusters '() Fringe (list (car Nodes)) Unvisited (cdr Nodes))
      (while Unvisited
         (let Cluster '()
            (push 'Fringe (pop 'Unvisited))
            (loop (NIL (car Fringe) 'done)
               (let (
                     XY (pop 'Fringe)
                     X (car XY)
                     Y (cadr XY)
                     Ch (get *Table Y X))
                  (push 'Cluster XY)
                  (when
                     (and
                        (not (member (list (dec X) Y) Fringe))
                        (not (member (list (dec X) Y) Cluster))
                        (= Ch (get *Table Y (dec X))))
                     (queue 'Fringe (list (dec X) Y))
                     (del (list (dec X) Y) 'Unvisited) )
                  (when
                     (and
                        (not (member (list (inc X) Y) Fringe))
                        (not (member (list (inc X) Y) Cluster))
                        (= Ch (get *Table Y (inc X))))
                     (queue 'Fringe (list (inc X) Y))
                     (del (list (inc X) Y) 'Unvisited) )
                  (when
                     (and
                        (not (member (list X (dec Y)) Fringe))
                        (not (member (list X (dec Y)) Cluster))
                        (= Ch (get *Table (dec Y) X)))
                     (queue 'Fringe (list X (dec Y)))
                     (del (list X (dec Y)) 'Unvisited) )
                  (when
                     (and
                        (not (member (list X (inc Y)) Fringe))
                        (not (member (list X (inc Y)) Cluster))
                        (= Ch (get *Table (inc Y) X)))
                     (queue 'Fringe (list X (inc Y)))
                     (del (list X (inc Y)) 'Unvisited) )))
            (when (not Cluster) (quit))
            (push 'Clusters Cluster)) )
      Clusters))

# A feladat első részében az a kérdés, hogy mekkora egy régió kerülete.
# Ezt úgy oldjuk meg, hogy minden pontjánál összeszámoljuk, hány olyan
# szomszédja van, mely nincs benne a régióban. Ezt összegezve megkapjuk
# a keresett számot. 

(de border_count (Node Points)
   (let (X (car Node) Y (cadr Node) Counter 0)
      (when (not (member (list (dec X) Y) Points)) (inc 'Counter))
      (when (not (member (list (inc X) Y) Points)) (inc 'Counter))
      (when (not (member (list X (dec Y)) Points)) (inc 'Counter))
      (when (not (member (list X (inc Y)) Points)) (inc 'Counter))
      Counter))

(de perimeter (Points)
   (apply + (make (for Node Points (link (border_count Node Points))))))

# Nincs más dolgunk, mint legenerálni a térkép dimenziói alapján a csúcsok halmazát.
# Azt régiókra bontani, majd a régiókon végighaladva kiszámolni azok "árát",
# és összegezni.

(de generate-nodes ()
   (make
      (for Y (length *Table)
         (for X (length (car *Table))
            (link (list X Y)) ))))

(de part1 (File)
   (read-input File)
   (let (Nodes (generate-nodes)
      Clusters (bfs Nodes)
      Counter 0)
      (for C Clusters
         (inc 'Counter (* (length C) (perimeter C))))
      (println Counter)))

# Part 2

# A feladat második részében nem a kerületet, hanem az oldalak számát kell
# meghatározni. Ehhez az előbbi "border_count" függvényt átalakítjuk,
# és nem számoljuk a külső oldalakat, hanem eltároljuk mindazzal,
# hogy merre külső.

(de border-points (Node Points)
   (let (X (car Node) Y (cadr Node) Border NIL)
      (when (not (member (list (dec X) Y) Points))
         (push 'Border (list X Y 'left)))
      (when (not (member (list (inc X) Y) Points))
         (push 'Border (list X Y 'right)))
      (when (not (member (list X (dec Y)) Points))
         (push 'Border (list X Y 'up)))
      (when (not (member (list X (inc Y)) Points))
         (push 'Border (list X Y 'down)))
      Border))

# Ezután az összegyűjtött darabokból összeválogatjuk az elsőket. Pl. a felső
# darabok esetén az egy sorban lévők közül a bal oldalit. Így minden oldalból
# csak egyet számolunk össze.

(de borders (Points)
   (let (Borders (make (for Node Points (chain (border-points Node Points))))
      Counter 0)
      (for Point Borders
         (when (and
            (= 'up (caddr Point))
            (not (member (list (inc (car Point)) (cadr Point) 'up) Borders)))
            (inc 'Counter) )
         (when (and
            (= 'down (caddr Point))
            (not (member (list (dec (car Point)) (cadr Point) 'down) Borders)))
            (inc 'Counter) )
         (when (and
            (= 'left (caddr Point))
            (not (member (list (car Point) (dec (cadr Point)) 'left) Borders)))
            (inc 'Counter) )
         (when (and
            (= 'right (caddr Point))
            (not (member (list (car Point) (inc (cadr Point)) 'right) Borders)))
            (inc 'Counter) ))
      Counter))

# A többi szinte ugyanaz mint korábban.

(de part2 (File)
   (read-input File)
   (let (Nodes (generate-nodes)
      Clusters (bfs Nodes)
      Counter 0)
      (for C Clusters
         (inc 'Counter (* (length C) (borders C))))
      (println Counter)))
