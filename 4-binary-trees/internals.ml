type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let internals tree =
    let rec internals_aux acc = function
        | Empty | Node(_, Empty, Empty) -> acc
        | Node(n, l, r) -> [n]@(internals_aux acc l)@(internals_aux acc r)
    in
    internals_aux [] tree
;;

let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)))
;;

assert (internals Empty = []) ;;
assert (internals (Node('a', Empty, Empty)) = []) ;;
assert (internals example_tree = ['a'; 'b'; 'c'; 'f']) ;;
