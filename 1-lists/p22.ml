let range i k =
    let rec range_helper i' k' acc =
        if i' = k' then 
            acc@[i']
        else if i' < k' then 
            range_helper (i' + 1) k' (acc@[i'])
        else 
            range_helper (i' - 1) k' (acc@[i'])
    in
    range_helper i k []
;; 

assert (range 4 9 = [4; 5; 6; 7; 8; 9]) ;;
assert (range 9 4 = [9; 8; 7; 6; 5; 4]) ;;
