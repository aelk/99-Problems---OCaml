let length lst =
  let rec len_helper acc = function
    | [] -> acc
    | hd::tl -> len_helper (acc + 1) tl
  in len_helper 0 lst

let length_fold lst =
  List.fold_left (fun a _ -> a + 1) 0 lst
