let rec gcd a b = if b = 0 then a else gcd b (a mod b) ;;

let coprime a b = gcd a b = 1 ;;

assert (coprime 13 27 = true) ;;
assert (coprime 20536 7826 = false) ;;
