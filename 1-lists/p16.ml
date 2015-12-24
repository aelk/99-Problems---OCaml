let drop lst n =
    let rec drop_helper count acc = function
        | [] -> acc
        | hd::tl -> if count = 1 then drop_helper n acc tl
                    else drop_helper (count - 1) (acc@[hd]) tl
    in
    drop_helper n [] lst
;;

assert (drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 =
        ["a"; "b"; "d"; "e"; "g"; "h"; "j"]) ;;
