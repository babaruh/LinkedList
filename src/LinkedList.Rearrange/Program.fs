type Node<'T> = 
    | Empty
    | Cons of 'T * Node<'T>

let rec rearrange list =
    match list with
    | Empty -> Empty
    | Cons (head, tail) ->
        match reverse tail with
        | Empty -> Cons (head, Empty)
        | Cons (last, rest) -> Cons (head, Cons (last, rearrange (reverse rest)))

and reverse list =
    let rec aux acc = function
    | Empty -> acc
    | Cons (head, tail) -> aux (Cons (head, acc)) tail
    in aux Empty list

let list1 = Cons (1, Cons (2, Cons (3, Cons (4, Empty))))
let list2 = Cons (1, Cons (2, Cons (3, Cons (4, Cons (5, Empty)))))

let result1 = rearrange list1
let result2 = rearrange list2

printfn $"%A{result1}"
printfn $"%A{result2}"
