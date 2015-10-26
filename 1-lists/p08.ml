let rec compress = function
  | []   -> []
  | [x]  -> [x]
  | h::t -> if List.hd t = h then compress t else h::(compress t)
;;

assert (compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"]) ;;
