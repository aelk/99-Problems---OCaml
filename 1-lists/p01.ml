let rec last = function
  | []     -> None
  | [x]    -> Some x
  | hd::tl -> last tl
;; 

assert (last [ "a" ; "b" ; "c" ; "d" ] = Some "d") ;;
assert (last [] = None) ;;
