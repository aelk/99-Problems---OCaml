let rec last_two = function
  | []       -> None
  | [x]      -> None
  | [x; y]   -> Some (x, y)
  | hd::tl   -> last_two tl
