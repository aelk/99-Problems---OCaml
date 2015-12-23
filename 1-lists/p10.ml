let encode lst =
    let rec encode_helper lst count acc =
        match lst with
        | [] -> acc
        | [el] -> acc@[(count + 1, el)]
        | hd::tl -> if hd = List.hd tl then encode_helper tl (count + 1) acc
                    else encode_helper tl 0 acc@[(count + 1, hd)]
    in
    List.rev (encode_helper lst 0 [])
;;

assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
        [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]) ;;
