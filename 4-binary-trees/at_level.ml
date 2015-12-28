type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec at_level tree level =
    match tree with
    | Empty -> []
    | Node(n, l, r) ->
        if level = 1 then [n]
        else (at_level l (level - 1))@(at_level r (level - 1))
;;

let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)))
;;

assert (at_level example_tree 2 = ['b'; 'c']) ;;
assert (at_level example_tree 5 = []) ;;
