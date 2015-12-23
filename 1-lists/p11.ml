type 'a rle =
    | One of 'a
    | Many of int * 'a
;;

let encode lst =
    let rec encode_helper lst count acc =
        match lst with
        | [] -> acc
        | [el] -> if count = 0 then acc@[One(el)]
                  else acc@[Many(count + 1, el)]
        | hd::tl -> if hd = List.hd tl then 
                        encode_helper tl (count + 1) acc
                    else if count = 0 then
                        encode_helper tl 0 acc@[One(hd)]
                    else
                        encode_helper tl 0 acc@[Many(count + 1, hd)]
    in
    List.rev (encode_helper lst 0 [])
;;

assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
        [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]) ;;
