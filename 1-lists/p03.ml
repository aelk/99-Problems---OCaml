let rec at k lst =
  match lst with
  | []     -> None
  | hd::tl -> if k = 1 then (Some hd) else at (k - 1) tl
;;

assert (at 3 [ 'a' ; 'b' ; 'c' ; 'd' ; 'e' ] = Some 'c') ;;
assert (at 3 [ 'a' ] = None) ;;
