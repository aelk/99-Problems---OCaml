let rec last = function
  | []     -> None
  | [x]    -> Some x
  | hd::tl -> last l
