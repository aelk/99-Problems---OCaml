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
  let rec phi_helper r acc =
    if r < m then
      phi_helper (r + 1) (acc + if coprime m r then 1 else 0)
    else acc
  in
  if m = 1 then 1 else phi_helper 1 0
;;
  
assert (phi 10 = 4) ;;
assert (phi 13 = 12) ;;
  
assert (phi2 10 = 4) ;;
assert (phi2 13 = 12) ;;
