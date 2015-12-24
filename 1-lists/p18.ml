let slice lst i k =
    let rec slice_helper i' k' acc = function
        | [] -> acc
        | hd::tl -> if i' = 0 && k' >= 0 then 
                        slice_helper i' (k' - 1) (acc@[hd]) tl
                    else
                        slice_helper (i' - 1) (k' - 1) acc tl
    in
    slice_helper i k [] lst
;;

assert ((slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6) =
        ["c"; "d"; "e"; "f"; "g"]) ;;
