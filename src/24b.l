# A feladat második részében hibakereséssel kell foglalkozni.
# Az áramkörben négy pár kapu helyet cserélt. Ha ezeket visszacseréljük, akkor
# egy 44 bites összeadó áramkörünk lesz. Utána lehetett volna nézni, hogy
# pontosan hogyan is néz ki egy ilyen áramkör, de maga a feladat inputja is
# elég sok információt ad erről.
# Mindenesetre olvassuk be az inputot, és dolgozzuk fel az második felét!
# Az első felét most nem használjuk, ezért eldobhatjuk.

(de read-input (File)
   (let (L "" V NIL Rules NIL)
      (in File
         (until (= (line) ""))

         (until (eof)
            (let V (mapcar pack (split (line) " "))
               (push 'Rules (cons (last V) (list (car V) (caddr V) (cadr V)))) )))
      Rules))

# A hibákat saját szemmel fogjuk felfedezni, ezért legyen rövid a kiírás,
# a kapukat jelölje csak egy betű!

(setq *OP-Code (list (cons "AND" "A") (cons "OR" "O") (cons "XOR" "X")) )

# Annak érdekében, hogy követhető legyen a kiírás, nevezzük el az áramkör egyes
# részeit!
# * x_i XOR y_i   => d_i
# * x_i AND y_i   => c_i
# * d_i AND c_i-1 => b_i
# * d_i AND a_i-1 => b_i
# * b_i OR  c_i   => a_i
# Ezért meg kell vizsgálnunk, hogy a cimkékben szereplő számok egymást követik,
# vagy esetleg egyenlőek:

(de second-carry (Label1 Label2)
   (let (
      SNumber1 (pack (cdr (chop Label1)))
      SNumber2 (pack (cdr (chop Label2)))
      Number1 (format SNumber1)
      Number2 (format SNumber2))
      (= Number1 (inc Number2)) ))

(de same-pair (List1 List2 Letter1 Letter2)
   (and
      (or
         (and (= (car List1) Letter1) (= (car List2) Letter2))
         (and (= (car List1) Letter2) (= (car List2) Letter1)))
      (= (cdr List1) (cdr List2)) ))

# Az egyes kapuk álneveit egy globális változóban tároljuk.
# Ha az előbb felsorolt feltételek valamely, az inputban szereplő szabályra
# teljesül, akkor ehhez a változóhoz hozzáadjuk az új elnevezést is.

(de name-nodes (Rule)
   (let (
      Name (car Rule)
      Left (chop (cadr Rule))
      Right (chop (caddr Rule))
      Lnick (cdr (assoc (cadr Rule) *Values))
      Rnick (cdr (assoc (caddr Rule) *Values))
      Op (cadddr Rule))
      (cond
         ( (assoc Name *Values) NIL)
         ( (and (= Op "XOR") (same-pair Left Right "x" "y"))
            (queue '*Values (cons Name (pack "d" (cdr Left)))) )
         ( (and (= Op "AND") (same-pair Left Right "x" "y"))
            (queue '*Values (cons Name (pack "c" (cdr Left)))) )
         ( (and (= Op "OR") (same-pair (chop Lnick) (chop Rnick) "b" "c"))
            (queue '*Values (cons Name (pack "a" (cdr (chop Lnick))))) )
         ( (and (= Op "AND") (second-carry Lnick Rnick)
            (pre? "d" Lnick) (or (pre? "c" Rnick) (pre? "a" Rnick)))
            (queue '*Values (cons Name (pack "b" (cdr (chop Lnick))))) )
         ( (and (= Op "AND") (second-carry Rnick Lnick)
            (pre? "d" Rnick) (or (pre? "c" Lnick) (pre? "a" Lnick)))
            (queue '*Values (cons Name (pack "b" (cdr (chop Rnick))))) ))))

# Továbbra a rekurzió emlegetése nélkül használom azt, szerencsére a hívási lánc
# nem olyan hosszú, hogy egy problémát okozna.
# Az (irányított) áramkört mint egy fát képzeljük el, és egy csúcsából
# felderítjük. Ha adott csúcsnak a fában már van beceneve, akkor azt írjuk ki,
# és nem megyünk tovább. Ellenkező esetben elkészítjük mind a két részfa szöveges
# reprezentációját, és szisztematikus sorrendben írjuk ki azokat, hogy
# következetes legyen az eredmény, a feladat készítőjének minden ármánykodása
# ellenére.

(de structure (Node)
   (let Nickname (cdr (assoc Node *Values))
      (if Nickname
         Nickname
         (let (
            NRule (assoc Node *Rules)
            Op (last NRule)
            Code (cdr (assoc Op *OP-Code))
            Left (structure (cadr NRule))
            Right (structure (caddr NRule)))
            (if (< (car Left) (car Right))
               (list Code Left Right)
               (list Code Right Left))))))

# Az előbbi függvényt az eredményt jelző, z-vel kezdődő csúcsokra alkalmazzuk.

(de show-structure (N)
   (for I N
      (println (structure (pack "z" (chop (pad 2 (- I 1))))))))

# Nincs más dolgunk, mint beolvasni az inputot, ezen jópárszor végigrágnunk
# magunkat, hogy minél több csúcsnak/kapunak tudjunk becenevet adni.
# Ha ezzel megvagyunk, akkor kiírhatjuk, hogy milyen szerkezete/előélete van
# egy-egy output-kapunak.

(de part2 (File)
   (let Rules (read-input File)
      (setq *Rules Rules *Values NIL)
      (do 150
         (mapcar name-nodes Rules))
      (show-structure 46)))

# Ahol megtörik a minta, ott kell keresni a hibát. A *Values asszociatív listában
# az assoc és rassoc segítségével lehet rátalálni egy becenév eredetijére, vagy
# egy csúcs becenevére. Nincs más dolgunk, mint a szerkezeti mintát követni,
# a feltalált hibát korrigálni az inputban. A következő futtatás már mutatja a
# következő hibát.