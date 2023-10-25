type Node<'T> = 
    | Empty
    | Cons of 'T * Node<'T>

let rec take n list =
    match (n, list) with
    | 0, _ -> Empty
    | _, Empty -> Empty
    | _, Cons(hd, tl) -> Cons(hd, take (n - 1) tl)

let rec drop n list =
    match (n, list) with
    | 0, _ -> list
    | _, Empty -> Empty
    | _, Cons(_, tl) -> drop (n - 1) tl

let rec length list =
    match list with
    | Empty -> 0
    | Cons(_, tl) -> 1 + length tl

let rec reverse list =
    let rec rev acc list =
        match list with
        | Empty -> acc
        | Cons(hd, tl) -> rev (Cons(hd, acc)) tl
    rev Empty list

let rec append list1 list2 =
    match list1 with
    | Empty -> list2
    | Cons(hd, tl) -> Cons(hd, append tl list2)

let rec rotate k list =
    if k > length list then list
    else let front = take k list
         let back = drop k list
         match back with
         | Empty -> reverse front
         | _ -> append (reverse front) (rotate k back)


let head1 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Empty)))))
let head2 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Empty)))))

let result1 = rotate 2 head1
let result2 = rotate 3 head2

printfn $"%A{result1}"
printfn $"%A{result2}"
