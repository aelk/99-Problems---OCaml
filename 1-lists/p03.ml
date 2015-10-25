let rec at k lst =
  match lst with
  | []     -> None
  | hd::tl -> if k = 1 then (Some hd) else at (k - 1) tl
