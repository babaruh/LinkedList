type Node<'T> = 
    | Empty
    | Cons of 'T * Node<'T>

let rec deleteNode value list =
    match list with
    | Empty -> Empty
    | Cons (x, xs) when x = value -> xs
    | Cons (x, xs) -> Cons (x, deleteNode value xs)

let list1 = Cons (4, Cons (5, Cons (1, Cons (9, Empty))))

let result1 = deleteNode 5 list1
let result2 = deleteNode 1 list1

printfn $"%A{result1}"
printfn $"%A{result2}"
