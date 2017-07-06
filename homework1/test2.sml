fun test (n : int) =
    let val s = [9,8,7,6,5,4,3,2,1]
    in
        let fun helper (nn : int, ss : int list) = 
            if nn = 1
            then hd ss
            else helper (nn - 1, tl ss)
        in
            helper (n, s)
        end
    end