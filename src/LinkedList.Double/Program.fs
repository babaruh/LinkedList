type Node<'T> = 
    | Empty
    | Cons of 'T * Node<'T>

let rec reverse list =
    match list with
    | Empty -> Empty
    | Cons (head, tail) -> append (reverse tail) (Cons (head, Empty))

and append list1 list2 =
    match list1 with
    | Empty -> list2
    | Cons (head, tail) -> Cons (head, append tail list2)

let doubleNumber list =
    let rec doubleWithCarry carry list =
        match list with
        | Empty when carry > 0 -> Cons (carry, Empty)
        | Empty -> Empty
        | Cons (head, tail) ->
            let sum = head * 2 + carry
            Cons (sum % 10, doubleWithCarry (sum / 10) tail)
    reverse (doubleWithCarry 0 (reverse list))

let list1 = Cons(1, Cons(8, Cons(9, Empty)))
let list2 = Cons(9, Cons(9, Cons(9, Empty)))

let result1 = doubleNumber list1
let result2 = doubleNumber list2

printfn $"%A{result1}"
printfn $"%A{result2}"
