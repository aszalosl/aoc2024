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

(de collect_firsts (N)
   (mapcar car (filter '((X)(= (cadr X) N)) *Orders))) 
 
(de correct_order (L)
   (if (cadr L)
      (let Ps (collect_firsts (car L))
          (if (sect Ps (cdr L))
              NIL 
             (correct_order (cdr L))))
      T))

(de middle (L)
   (let Len (length L)
        (format (car (nth L (+ 1 (/ Len 2)))))))

(de part1 (F)
  (load_data F)
  (apply + (mapcar middle (filter correct_order *Updates))))

# part 2 ----------------

(de foregoing (X L)
   (let F (collect_firsts X)
       (sect F L)))

(de middle_ordered (L)
    (middle
       (mapcar cadr
          (sort 
             (make 
                (for X L (link (list (length (foregoing X L)) X))))))))

(de part2 ()
    (apply + (mapcar middle_ordered 
       (filter '((X)(not (correct_order X))) *Updates))) )
