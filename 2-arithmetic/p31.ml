let is_prime n =
  let n = max n (-n) in
  if n mod 2 = 0 then (
    false;
  )
  else
    let rec is_prime_helper div =
        div * div > n || n mod div <> 0 && is_prime_helper (div + 2)
    in
    n <> 1 && is_prime_helper 3
;;
  
assert (is_prime 1 = false) ;;
assert (is_prime 7 = true) ;;
assert (is_prime 12 = false) ;;
