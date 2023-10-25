type Node<'T> = 
    | Empty
    | Cons of 'T * Node<'T>

let rec mergeLists list1 list2 =
    match list1, list2 with
    | Empty, _ -> list2
    | _, Empty -> list1
    | Cons(head1, tail1), Cons(head2, _) when head1 <= head2 -> Cons(head1, mergeLists tail1 list2)
    | Cons _, Cons(head2, tail2) -> Cons(head2, mergeLists list1 tail2)
    

let list1 = Cons(1, Cons(2, Cons(4, Empty)))
let list2 = Cons(1, Cons(3, Cons(4, Empty)))
let list3: Node<int> = Empty
let list4: Node<int> = Empty
let list5 = Cons(0, Empty)

let result1 = mergeLists list1 list2
printfn $"%A{result1}"

let result2 = mergeLists list3 list4
printfn $"%A{result2}"  

let result3 = mergeLists list3 list5
printfn $"%A{result3}"

