let duplicate lst =
    let rec duplicate_helper lst acc =
        match lst with
        | [] -> acc
        | hd::tl -> duplicate_helper tl (acc@[hd; hd])
    in
    duplicate_helper lst []
;;

assert (duplicate ["a";"b";"c";"c";"d"] =
        ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]) ;;
