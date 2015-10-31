let rec gcd a b = if b = 0 then a else gcd b (a mod b) ;;

let coprime a b = gcd a b = 1 ;;

let phi m =
  let count = ref 0 in
  for i = 0 to m do
    if coprime m i then incr count
  done;
  !count
;;

let phi2 m =
  let int_of_bool b = if b then 1 else 0 in
  let rec phi_helper m r acc =
    match r with
    | 0 -> acc
    | _ -> phi_helper m (r - 1) (acc + int_of_bool (coprime m r))
  in
  phi_helper m (m - 1) 0
;;
  
assert (phi 10 = 4) ;;
assert (phi 13 = 12) ;;
  
assert (phi2 10 = 4) ;;
assert (phi2 13 = 12) ;;
