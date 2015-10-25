let rev lst =
  let rec rev_helper acc lst =
    match lst with
    | []     -> acc
    | hd::tl -> rev_helper (hd::acc) tl
  in rev_helper [] lst

let rev_fold lst =
  List.fold_left (fun hd tl -> tl::hd) [] lst
