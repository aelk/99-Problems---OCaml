let rec last_two = function
  | []       -> None
  | [x]      -> None
  | [x; y]   -> Some (x, y)
  | hd::tl   -> last_two tl
;;

assert (last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d")) ;;
assert (last_two [ "a" ] = None) ;;
