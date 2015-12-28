type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec internals = function
    | Empty | Node(_, Empty, Empty) -> []
    | Node(n, l, r) -> [n]@(internals l)@(internals r)
;;

let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)))
;;

assert (internals Empty = []) ;;
assert (internals (Node('a', Empty, Empty)) = []) ;;
assert (internals example_tree = ['a'; 'b'; 'c'; 'f']) ;;
