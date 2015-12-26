let insert_at elem pos lst =
    let rec insert_at el n acc = function
        | [] -> if n = pos then acc@[elem] else acc
        | hd::tl -> if n = pos then 
                        insert_at elem (n + 1) (acc@[elem]) (hd::tl)
                    else
                        insert_at elem (n + 1) (acc@[hd]) tl
    in
    insert_at elem 0 [] lst
;;

assert (insert_at "alfa" 1 ["a";"b";"c";"d"] =
        ["a"; "alfa"; "b"; "c"; "d"]) ;;
assert (insert_at "alfa" 3 ["a";"b";"c";"d"] =
        ["a"; "b"; "c"; "alfa"; "d"]) ;;
assert (insert_at "alfa" 4 ["a";"b";"c";"d"] =
        ["a"; "b"; "c"; "d"; "alfa"]) ;;
