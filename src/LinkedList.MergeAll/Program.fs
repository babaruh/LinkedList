type Node<'T> = 
    | Empty
    | Cons of 'T * Node<'T>

let rec mergeLists list1 list2 =
    match list1, list2 with
    | Empty, _ -> list2
    | _, Empty -> list1
    | Cons(head1, tail1), Cons(head2, _) when head1 <= head2 -> Cons(head1, mergeLists tail1 list2)
    | Cons _, Cons(head2, tail2) -> Cons(head2, mergeLists list1 tail2)

let rec mergeKLists lists =
    match lists with
    | [] -> Empty
    | [h] -> h
    | _ ->
        let mid = List.length lists / 2
        let l1 = List.take mid lists
        let l2 = List.skip mid lists
        mergeLists (mergeKLists l1) (mergeKLists l2)

let rec printList list =
    match list with
    | Empty -> "[]"
    | Cons(head, tail) -> 
        let tailStr = printList tail
        match tailStr with
        | "[]" -> $"[%A{head}]"
        | _ -> $"[%A{head}; %s{tailStr.Trim('[', ']')}]"

let list1 = Cons(1, Cons(4, Cons(5, Empty)))
let list2 = Cons(1, Cons(3, Cons(4, Empty)))
let list3 = Cons(2, Cons(6, Empty))
let list4: Node<int> = Empty
let lists = [list1; list2; list3]
let lists1: Node<int> list = []
let lists2 = [list4]

let result = mergeKLists lists
let result1 = mergeKLists lists1
let result2 = mergeKLists lists2

printfn $"%s{printList result}"
printfn $"%s{printList result1}"
printfn $"%s{printList result2}"
