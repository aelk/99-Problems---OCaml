type 'a mult_tree = T of 'a * 'a mult_tree list;;

let rec count_nodes = function
    | T(_, []) -> 1
    | T(_, tree_list) -> 
        1 + (List.fold_left (+) 0 (List.map count_nodes tree_list))
;;

assert (count_nodes (T('a', [T('f',[]) ])) = 2) ;;
assert (count_nodes (T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])])) = 7) ;;
