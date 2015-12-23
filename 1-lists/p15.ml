let replicate lst occurrences =
    let make_list elem occurrences' =
        let rec make_list_helper acc = function
            | 0 -> acc
            | count -> make_list_helper (elem::acc) (count - 1)
        in
        make_list_helper [] occurrences'
    in
    let rec replicate_helper acc = function
        | [] -> acc
        | hd::tl -> replicate_helper (acc@(make_list hd occurrences)) tl
    in
    replicate_helper [] lst
;;

assert (replicate ["a";"b";"c"] 3 = 
        ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]) ;;
