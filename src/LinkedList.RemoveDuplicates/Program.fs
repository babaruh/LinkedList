type Node<'T> = 
    | Empty
    | Cons of 'T * Node<'T>
    
let rec removeDuplicates list =
    match list with
    | Empty -> Empty
    | Cons (head, Empty) -> Cons (head, Empty)
    | Cons (head1, Cons (head2, tail)) when head1 = head2 -> removeDuplicates (Cons (head2, tail))
    | Cons (head, tail) -> Cons (head, removeDuplicates tail)

let list1 = Cons (1, Cons (1, Cons (2, Empty)))
let list2 = Cons (1, Cons (1, Cons (2, Cons (3, Cons (3, Empty)))))

let result1 = removeDuplicates list1
let result2 = removeDuplicates list2

printfn $"%A{result1}"
printfn $"%A{result2}"
