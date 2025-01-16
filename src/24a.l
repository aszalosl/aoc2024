# Az utolsó előtti nap feladata egy áramkörhöz kapcsolódik. Az input két részből
# áll, a bemeneti értékekből, illetve a logikai kapuk szerkezetéből.
# Ennek megfelelően két részből áll a beolvasás is.

(de read-input (File)
   (let (L "" V NIL)
      (in File
         (setq *Values NIL)
         (until (= (setq L (line)) "")
            (setq V (mapcar pack (split L ":")))
            (push '*Values (cons (car V) (format (cadr V)))) )

         (setq *Rules NIL)
         (until (eof)
            (let V (mapcar pack (split (line) " "))
               (push '*Rules (cons (last V) (list (car V) (caddr V) (cadr V)))) )))))

# A három logikai kapu viselkedését szimulálni kell, tehát össze kell kapcsolni
# a megfelelő függvényeket azok szöveges alakjával, és alkalmazni az bemenő
# paraméterekre. Az eredményt a célváltozó nevével párban eltároljuk a *Values
# globális változóban, ami egy asszociatív lista.

(de calculate-value (N Left Right OP)
   (when (= OP "AND")
      (push '*Values
         (cons N (& Left Right))))
   (when (= OP "OR")
      (push '*Values
         (cons N (| Left Right))))
   (when (= OP "XOR")
      (push '*Values
         (cons N (x| Left Right)))) )

# Miután függési viszonyok vannak az egyes változók között, egy irányított gráfot
# készíthetünk az által, hogy mely változókra van szükség egy adott változó
# kiértékeléséhez. Topológikus rendezés segítené a gyorsabb feldolgozást,
# amit például egy mélységi kereséssel is megoldhatunk. (Valójában a *Values
# tekinthető a rendezés eredményének, csak éppen fordított irányban.
# Természetesen egy veremmel dolgozunk, melybe bedobtunk a gráf egy csúcsát.
# Ha a verem tetején lévő elem értékének kiszámításához már minden szerepel a
# *Values-ban, akkor kész is vagyunk. Ha nem, akkor ez az elem marad itt a
# veremben, s fölérakjuk a kiszámításához szükséges két változót, s folytatjuk
# a végtelen ciklust. Ebből csak akkor menekülhetünk, ha kiürült a verem.


(de dfs (Stack)
   (loop
      (NIL Stack)
      (let (
         N (pop 'Stack)
         NRule (assoc N *Rules)
         Left (cadr NRule)
         Right (caddr NRule)
         OP (last NRule)
         LeftPair (assoc Left *Values)
         RightPair (assoc Right *Values))
         (if (and LeftPair RightPair)
            (calculate-value N (cdr LeftPair) (cdr RightPair) OP)
            (push 'Stack N)
            (when (not LeftPair) (push 'Stack Left))
            (when (not RightPair) (push 'Stack Right)) ))))

# Mivel elképzelhető, hogy a feladatban szereplő mátrix nem összefüggő,
# a mélységi keresést eseetleg többször is le kell futtatnunk. A biztonság
# kedvéért az összes logikai kaput végigellenőrizzük.

(de calculate-values (Nodes)
      (for N Nodes
            (when (not (assoc N '*Values))
               (dfs (list N)))))

# Az eredmény egy bináris szám - pontosabban a számjegyeinek a listája,
# amit át kell váltani tizes számrendszerbe. Itt a standard módszert
# implementáljuk.

(de bit-to-number (L)
   (let V 0
      (for B L
         (setq V (+ B (* 2 V))))
      V))

# Az első rész a z-betűs változók értékére kíváncsi, így a szűréshez tartozó
# függvényt most külön definiáljuk.

(de begins-with-zed (W) (pre? "z" W))

# Ezek után már nincs más dolgunk, mint szépen sorban meghívjuk a korábban
# definiált függvényeket.

(de part1 (File)
   (read-input File)
   (calculate-values (mapcar car *Rules))
   (let (
      Zs  (filter begins-with-zed *Values)
      Ordered  (sort Zs)
      Bits (reverse (mapcar cdr Ordered))
      Result (bit-to-number Bits))
      (println Result)))
