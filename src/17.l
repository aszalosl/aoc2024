# A 17. feladatban egy speciális processzor futását kell nyomon követni.
# Maga a processzor igen egyszerű, három regiszterrel és egy utasításszámlálóval
# rendelkezik. Ezek alkotják az állapotát, és ahelyett, hogy mágikus konstansokat
# használnék, vezessünk be rá nevesített konstansokat!

(setq IP 1 RegA 2 RegB 3 RegC 4)

# Kétfajta címzésünk van, a bonyolultabbhoz készítünk egy segédfüggvényt.

(de combo (Operand State)
   (let R 0
      (when (< Operand 4) (setq R Operand))
      (when (= Operand 4) (setq R (get State RegA)))
      (when (= Operand 5) (setq R (get State RegB)))
      (when (= Operand 6) (setq R (get State RegC)))
      # (prin "   c: " R " ")
      R))

# A processzornak csupán nyolc utasítása van, de eléggé bonyolultak.
# Az alábbi függvény egy lépés hajt végre. Átadjuk neki a gépünk egy állapotát,
# és az új állapotot kapjuk vissza.

(de step (Opcode Operand State)
   (let R 0
      (when (= Opcode 0) # adv
         (let (
            Denom (** 2 (combo Operand State))
            RegisterA (get State RegA)
            Result (/  RegisterA Denom) )
            (setq R
               (list (+ (car State) 2) Result (get State RegB) (get State RegC)) )))
      (when (= Opcode 6) # bdv
         (let (
            Denom (** 2 (combo Operand State))
            RegisterA (get State RegA)
            Result (/ RegisterA Denom) )
            (setq R
               (list (+ (get State IP) 2) (get State RegA) Result (get State RegC)) )))
      (when (= Opcode 7) # cdv
         (let (
            Denom (** 2 (combo Operand State))
            RegisterA (get State RegA)
            Result (/ RegisterA Denom) )
            (setq R
               (list (+ (car State) 2) (get State RegA) (get State RegB) Result) )))
      (when (= Opcode 1) # bxl
         (let Result (x| (get State RegB) Operand)
            (setq R
               (list (+ (car State) 2) (get State RegA) Result (get State RegC)) )))
      (when (= Opcode 2) # bst
         (let Result (% (combo Operand State) 8)
            (setq R
               (list (+ (car State) 2) (get State RegA) Result (get State RegC)) )))
      (when (= Opcode 3) # jnz
         (if (=0 (get State RegA))
            (setq R (list
               (+ (car State) 2) (get State RegA) (get State RegB) (get State RegC)) )
            (setq R (list
               Operand (get State RegA) (get State RegB) (get State RegC)) )))
      (when (= Opcode 4) # bxc
         (let Result (x| (get State RegB) (get State RegC))
            (setq R (list (+ (car State) 2) (get State RegA) Result (get State RegC)) )))
      (when (= Opcode 5) # out
         (let Result (% (combo Operand State) 8)
            (queue '*Out Result)
            (setq R (list
               (+ (car State) 2) (get State RegA) (get State RegB) (get State RegC)) )))
      # (prin " -> ") (print R) (prinl)
      R ))


# A futás számunkra nem jelent mást, mint az előbbi függvényt hívjuk mindaddig,
# amíg ki nem futunk a programból, azaz az utasításszámláló által mutatott
# területen már nincs semmi.

(de run (St)
   (let State St
      (loop
         (T (= NIL (get *Prg (inc (car State)))) (println *Out))
         (let (
            IP (car State)
            OpCode (get *Prg (inc IP))
            Operand (get *Prg (+ IP 2)) )
            # (prin "run> C: " OpCode "  O: " Operand "  S: ") (print State)
            (setq State (step OpCode Operand State)) ))))

# Egyszerű listaként adhatjuk meg a teszt-programunkat, míg a kezdőállapotból
# csak az A regiszter állapota fontos, legalábbis az én esetemben.

(setq *Prg (list 0 1 5 4 3 0))
(de part1 (A)
   (setq *Out Nil)
   (run (list 0 A 0 0)))

# Part 2
# Nem tudom, hogy mennyire személyre szólóak a programok, viszont
# mivel önmagát kiíró kódot kell produkálni, meglehet, hogy másnak semmit
# sem ér az alábbi kód.
#A programot alaposabban megfigyelve az A regiszteren múlik a másik két
#regiszter tartalma, illetve az output is. A program végén az A regiszter értéke
#0 lesz - ez kell, hogy a program leálljon. Mivel nyolccal elosztjuk A-t menet
#közben, így a cilkusba annak 0 és 7 közötti értékkel kell belépni.
#Ezek mindegyikére teszteljük a kimenetet, és ami illeszkedett, azt feljegyezzük.
#Miután ide egy korábbi nyolcas osztással kerültünk, a feljegyzett szám
#nyolcszorosával, illetve annak környezetével kell dolgoznunk.
#Mivel a leképezés nem bijektív, több értékre is kaphatjuk ugyanazt a kimenetet,
#illetve ha ezek közül rosszul választunk, akkor zsákutcába juthatunk.
#Amiatt már megint szélességi keresést fogunk használni, ahol a Fringe most a
#Candiates nevet kapta.

(de part2 ()
   (let Candidates (list 0)
      (loop
         (T (= NIL (car Candidates)) (quit))
         (let A (pop 'Candidates)
            (for X (range 0 7)
               (let Result (part1 (+ A X))
                  (when
                     (pre? (reverse Result) (reverse *Prg))
                     (queue 'Candidates (* 8 (+ A X)))
                     #(println (+ A X))
                     )
                  (when (= Result *Prg)
                     (println (+ A X))
                     (quit))))))))