let remove_at k lst =
    let rec remove_at_helper acc n = function
        | [] -> acc
        | hd::tl -> if n = k then remove_at_helper acc (n + 1) tl
                    else remove_at_helper (acc@[hd]) (n + 1) tl
    in
    remove_at_helper [] 0 lst
;;

assert (remove_at 1 ["a";"b";"c";"d"] = ["a"; "c"; "d"]) ;;
