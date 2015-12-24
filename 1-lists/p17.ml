let split lst len_first =
    let rec split_helper len acc = function
        | [] -> acc
        | hd::tl -> let lft, rgt = acc in
                    if len >= 1 then split_helper (len - 1) (lft@[hd], rgt) tl
                    else split_helper len (lft, rgt@[hd]) tl
    in split_helper len_first ([], []) lst
;;

assert (split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 =
        (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])) ;;
assert (split ["a";"b";"c";"d"] 5 = (["a"; "b"; "c"; "d"], [])) ;;
