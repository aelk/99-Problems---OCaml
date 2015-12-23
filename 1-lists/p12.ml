type 'a rle =
    | One of 'a
    | Many of int * 'a
;;

let decode lst =
    let make_list elem occurrences =
        let rec make_list_helper acc = function
            | 0 -> acc
            | count -> make_list_helper (elem::acc) (count - 1)
        in
        make_list_helper [] occurrences
    in
    let rec decode_helper lst acc =
        match lst with
        | [] -> acc
        | One el::tl -> decode_helper tl (acc@[el])
        | Many (count, el)::tl -> decode_helper tl (acc@(make_list el count))
    in
    decode_helper lst []
;;

assert (decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] =
        ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) ;;
