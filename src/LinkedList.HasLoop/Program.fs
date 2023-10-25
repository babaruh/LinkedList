type Node<'T> = 
    | Empty
    | Cons of 'T * Node<'T>

let hasLoop list =
    let rec loopDetect slow fast =
        match slow, fast with
        | Cons _, Cons(_, Cons _) when slow = fast -> true
        | Cons(_, nextSlow), Cons(_, Cons(_, nextFast)) -> loopDetect nextSlow nextFast
        | _ -> false
    loopDetect list list
    
let list1 = Cons(3, Cons(2, Cons(0, Cons(-4, Empty))))
let list2 = Cons(1, Cons(2, Empty))
let list3 = Cons(1, Empty)

let result1 = hasLoop list1
let result2 = hasLoop list2
let result3 = hasLoop list3

printfn $"%A{result1}"
printfn $"%A{result2}"
printfn $"%A{result3}"
