type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let count_leaves tree =
    let rec count_leaves_aux acc = function
        | Empty -> acc
        | Node(_, Empty, Empty) -> acc + 1
        | Node(_, l, r) -> count_leaves_aux acc l + count_leaves_aux acc r
    in
    count_leaves_aux 0 tree
;;

let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)))
;;

assert (count_leaves Empty = 0) ;;
assert (count_leaves example_tree = 3) ;;
