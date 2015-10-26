type 'a node =
  | One of 'a 
  | Many of 'a node list
;;

let rec flatten = function
  | []           -> []
  | (One x)::tl  -> x::flatten tl
  | (Many x)::tl -> (flatten x)@(flatten tl)
;;

let flatten2 lst =
  let rec flatten2_helper acc = function
    | []           -> acc
    | (One x)::tl  -> flatten2_helper (x::acc) tl
    | (Many l)::tl -> flatten2_helper (flatten2_helper acc l) tl
  in List.rev (flatten2_helper [] lst)
;;

assert (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"]) ;;
assert (flatten2 [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"]) ;;
