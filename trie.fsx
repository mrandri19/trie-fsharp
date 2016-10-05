// new Trie()
// Trie.add string
// Trie.findPrefix string -> Option<string list>
// Trie.remove string

// UTILITY
let assertEqual lhs rhs =
    if lhs <> rhs then
        failwith <| sprintf "%A is different from %A\n" lhs rhs
        ()
    else ()

let l x = printfn "%A" x


open System

type Trie =
    | Leaf of char
    | Node of char * Trie list

// HELPERS
let rec create (word:char seq): Trie =
    match Seq.length word with
    | 0 -> raise (ArgumentException "No empty string")
    | 1 -> Leaf (Seq.head word)
    | x -> Node (Seq.head word, [create (Seq.tail word)])

module List =
    let replace findFn (changeElemFn: Trie -> Trie) (list: Trie list): Trie list =
        let f elem =
            if findFn elem then
                changeElemFn elem
            else elem
        List.map f list

let notContains letter (xs: Trie list) =
    let f x =
        match x with
        | Leaf c -> c = letter
        | Node (c,_) -> c = letter
    (List.tryFind f xs) = None

// /HELPERS

let rec insert word trie =
    if Seq.isEmpty word then
        raise (ArgumentException "No empty string")

    let initialChar = match trie with
                      | Leaf c -> c
                      | Node(c,_) -> c

    if (Seq.head word) <> initialChar then
        raise (ArgumentException "String must start with the same letter")

    match trie with
    | Leaf c ->
        match Seq.length word with
        | 1 -> trie
        | x -> Node (c, [create (Seq.tail word)])
    | Node(c, nodelist) ->
        match Seq.length word with
        | 1 -> trie
        | x ->
            if not (notContains (word |> Seq.tail |> Seq.head) nodelist) then
                Node (
                        c,
                        (
                            List.replace (fun n ->
                                match n with
                                | Leaf c -> c = (word |> Seq.tail |> Seq.head)
                                | Node (c,_) -> c = (word |> Seq.tail |> Seq.head)
                            ) (fun n -> insert (Seq.tail word) n) nodelist
                        )
                    )
            else
                Node (c, nodelist @ [create (Seq.tail word)]) // CASE D


l "TESTING: create"
create "a" |> l
create "ab" |> l

printfn ""
printfn ""
printfn ""
printfn ""

l "TESTING: insert"
create "a" |> insert "a" |> l
create "a" |> insert "ab" |> l

create "ab" |> insert "a" |> l
create "ab" |> insert "ab" |> l
create "ab" |> insert "abc" |> l


create "ab" |> insert "ad" |> l
create "abc" |> insert "ad" |> l
create "abc" |> insert "abd" |> l
