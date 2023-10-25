type Node<'T> = 
    | Empty
    | Cons of 'T * Node<'T>

let splitList (head: Node<int>) (x: int) =
    let rec helper node less greater =
        match node with
        | Empty -> (less, greater)
        | Cons(h, t) ->
            if h < x then helper t (Cons(h, less)) greater
            else helper t less (Cons(h, greater))
    let less, greater = helper head Empty Empty

    let rec reverse node acc =
        match node with
        | Empty -> acc
        | Cons(h, t) -> reverse t (Cons(h, acc))
    let less = reverse less Empty
    let greater = reverse greater Empty

    let rec concat l1 l2 =
        match l1 with
        | Empty -> l2
        | Cons(h, t) -> Cons(h, concat t l2)
    concat less greater

let head1 = Cons(1, Cons(4, Cons(3, Cons(2, Cons(5, Cons(2, Empty))))))
let x1 = 3
printfn $"%A{splitList head1 x1}"

let head2 = Cons(2, Cons(1, Empty))
let x2 = 2
printfn $"%A{splitList head2 x2}"
