// UTILITY
let assertEqual lhs rhs =
    if lhs <> rhs then
        failwith <| sprintf "actual: %A is different from expected: %A\n" lhs rhs
        ()
    else ()

let l x = printfn "%A" x
// UTILITY

open System

type Trie =
    | Leaf of char
    | Node of char * bool * Trie list
    // The bool reperesents a word boundary:
    // if I insert 'a' in 'ab' and then call strings on the trie
    // I will only get 'ab' out. By having a boolean that represents
    // a word boundary we solve this problem.

let rec create (word:char seq): Trie =
    match Seq.length word with
    | 0 -> raise (ArgumentException "No empty string")
    | 1 -> Leaf (Seq.head word)
    | x -> Node (Seq.head word, false, [create (Seq.tail word)])

// HELPERS
module List =
    let replace findFn changeElemFn list =
        let f elem =
            if findFn elem then
                changeElemFn elem
            else elem
        List.map f list

let contains letter (xs: Trie list) =
    let f x =
        match x with
        | Leaf c -> c = letter
        | Node (c, _, _) -> c = letter
    (List.tryFind f xs) <> None

let find (letter: char) (xs: Trie list) =
    let f x =
        match x with
        | Leaf c -> c = letter
        | Node (c, _, _) -> c = letter
    (List.tryFind f xs)
// HELPERS

let rec insert word trie =
    if Seq.isEmpty word then
        raise (ArgumentException "No empty string")

    let initialChar = match trie with
                      | Leaf c -> c
                      | Node(c, _, _) -> c

    if (Seq.head word) <> initialChar then
        raise (ArgumentException "String must start with the same letter")

    match trie with
    | Leaf c ->
        match Seq.length word with
        | 1 -> trie
        | x -> Node (c, true, [create (Seq.tail word)])
    | Node(c, wordBoundary, nodeList) ->
        match Seq.length word with
        | 1 -> Node(c, true, nodeList)
        | x ->
            if (contains (word |> Seq.tail |> Seq.head) nodeList) then
                Node (
                        c,
                        wordBoundary,
                        (
                            List.replace (fun n ->
                                match n with
                                | Leaf c -> c = (word |> Seq.tail |> Seq.head)
                                | Node (c, _, _) -> c = (word |> Seq.tail |> Seq.head)
                            ) (fun n -> insert (Seq.tail word) n) nodeList
                        )
                    )
            else
                Node (c, wordBoundary, nodeList @ [create (Seq.tail word)]) // CASE D


let rec exists word trie =
    if Seq.isEmpty word then
        raise (ArgumentException "No empty string")

    match trie with
    | Leaf c ->
        if Seq.length word = 1 then
            Seq.head word = c
        else false
    | Node (c, _, nodeList) ->
        if c = Seq.head word then
            match Seq.length word with
            | 1 -> true
            | x ->
                match (find (word |> Seq.tail |> Seq.head) nodeList) with
                | None -> false
                | Some(n) -> exists (Seq.tail word) n

        else false


let rec findByPrefix (word) (trie): Trie list option =
    if Seq.isEmpty word then
        raise (ArgumentException "No empty string")

    match trie with
    | Leaf c ->
        if Seq.head word = c && (Seq.length word = 1) then
            [Leaf c] |> Some
        else None
    | Node (c, _, nodeList) ->
         if Seq.head word = c then
            match Seq.length word with
            | 1 -> Some nodeList
            | x ->
                match (find (word |> Seq.tail |> Seq.head) nodeList) with
                | None -> None
                | Some n -> findByPrefix (Seq.tail word) n
         else None


let strings trie =
    let results = new ResizeArray<string>()
    let leafFn s =
        results.Add s
        ()

    let rec internalFn acc (trie) =
        match trie with
        | Leaf c -> leafFn (acc + c.ToString())
        | Node (c, wordBoundary, nodeList) ->
            if wordBoundary then
                leafFn (acc + c.ToString())
            List.iter (internalFn <| acc + c.ToString()) nodeList

    internalFn "" trie
    results.ToArray() |> Array.toList



// TESTS
// create
assertEqual (create "a")      (Leaf 'a')
assertEqual (create "ab")     (Node ('a', false, [Leaf 'b']))

// insert
assertEqual (create "a" |> insert "a")        (Leaf 'a')

assertEqual (create "ab" |> insert "a")       (Node ('a', true, [Leaf 'b']))
assertEqual (create "ab" |> insert "ab")      (Node ('a', false, [Leaf 'b']))
assertEqual (create "ab" |> insert "abc")     (Node ('a', false, [Node ('b', true, [Leaf 'c'])]))
assertEqual (create "ab" |> insert "ad")      (Node ('a', false, [Leaf 'b'; Leaf 'd']))
assertEqual (create "abc" |> insert "ad")     (Node ('a', false, [Node ('b', false, [Leaf 'c']); Leaf 'd']))
assertEqual (create "abc" |> insert "abd")    (Node ('a', false, [Node ('b', false, [Leaf 'c'; Leaf 'd'])]))

// exists
assertEqual (create "a" |> exists "a")                     true
assertEqual (create "a" |> exists "b")                     false
assertEqual (create "a" |> exists "ab")                    false
assertEqual (create "ab" |> exists "a")                    true
assertEqual (create "ab" |> exists "b")                    false
assertEqual (create "ab" |> exists "ab")                   true
assertEqual (create "ab" |> exists "abc")                  false
assertEqual (create "abc" |> insert "abd" |> exists "ab")  true
assertEqual (create "abc" |> insert "abd" |> exists "abc") true
assertEqual (create "abc" |> insert "abd" |> exists "abd") true

// findByPrefix
assertEqual (create "a" |> findByPrefix "a")    (Some [Leaf 'a'])
assertEqual (create "a" |> findByPrefix "ab")   None
assertEqual (create "a" |> findByPrefix "b")    None
assertEqual (create "ab" |> findByPrefix "b")   None
assertEqual (create "abc" |> findByPrefix "b")  None

assertEqual (create "ab" |> findByPrefix "a")                                                       (Some <| [Leaf 'b'])
assertEqual (create "ab" |> insert "ad" |> findByPrefix "a")                                        (Some <| [Leaf 'b'; Leaf 'd'])
assertEqual (create "abc" |> insert "abd" |> findByPrefix "ab")                                     (Some <| [Leaf 'c'; Leaf 'd'])
assertEqual (create "abc" |> insert "abd" |> insert "ace" |> insert "acf" |> findByPrefix "ab")     (Some <| [Leaf 'c'; Leaf 'd'])
assertEqual (create "abc" |> insert "abd" |> insert "ace" |> insert "acf" |> findByPrefix "ac")     (Some <| [Leaf 'e'; Leaf 'f'])

// strings
assertEqual (create "abc" |> strings) ["abc"]
assertEqual (create "a" |> insert "ab" |> insert "abc" |> insert "abd" |> insert "ace" |> insert "acf" |> strings) ["a"; "ab"; "abc"; "abd"; "ace"; "acf";]

let dictionary = ["ab"; "abc"; "ac"; "abd"] in
    let trie = List.fold (fun trie word -> insert word trie) (create "a") dictionary in
        assertEqual (trie |> strings |> List.length) (dictionary.Length + 1)

create "abcdefgh"
|> insert "abd"
|> insert "ace"
|> insert "acf"
|> findByPrefix "ab"
|> Option.get
|> List.collect strings
|> List.map ((+) "ab")
|> l