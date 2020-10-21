(kAS as public) in 
(kSB as public) in 
(cSB as public) in
(cAB as public) in 
(cAS as public) in 
(
  (cAS |< [x] in case x of {y} (kAS) in cSB |> [{y}(kSB)] in nil) |
  ((kAB as public) in cAS |> [ {kAB}(kAS) ] in cAB |> [{nil}(kAB)] in nil ) |
  (cSB |< [x] in case x of {y}(kSB) in cAB |< [z] in case z of {w} (y) in nil)
)