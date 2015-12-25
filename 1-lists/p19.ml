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

let rotate lst n =
    let len = List.length lst in
    if n = 0 then lst
    else if n > 0 then (slice lst n len)@(slice lst 0 (n - 1))
    else (slice lst (len + n) len)@(slice lst 0 (len + n - 1))
;;

assert ((rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3) =
        ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]) ;;
assert ((rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2)) =
        ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]) ;;
