(some as any) in
(key as secret) in 
(n as public) in
(( key |< [x1,x2,x3] in x1 |> [x1,x2,x3] in nil) | 
(!(n |< [x1] in let (x2,x3) = {key}(x1) in x2 |> [x3,some,succ(nil)] in nil)))