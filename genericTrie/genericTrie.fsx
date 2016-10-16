// UTILITY
let assertEqual lhs rhs =
    if lhs <> rhs then
        failwith <| sprintf "actual: %A is different from expected: %A\n" lhs rhs
        ()
    else ()

let l x = printfn "%A" x
// UTILITY

open System

// Associate a generic object with a word boundary
// eg. an URL
type WordBoundaryData<'T> =
    | NotEOW
    | EOW of 'T

type Trie<'T> =
    | Leaf of char * 'T
    | Node of char * WordBoundaryData<'T> * Trie<'T> list
    // The WordBoundyData reperesents a word boundary:
    // if I insert 'a' in 'ab' and then call strings on the trie
    // I will only get 'ab' out. By having a type that represents
    // a word boundary we solve this problem.

let rec create<'T> (word:char seq) (data:'T): Trie<'T> =
    match Seq.length word with
    | 0 -> raise (ArgumentException "No empty string")
    | 1 -> Leaf (Seq.head word, data)
    | x -> Node (Seq.head word, NotEOW, [create (Seq.tail word) data])

// HELPERS
module List =
    let replace findFn changeElemFn list =
        let f elem =
            if findFn elem then
                changeElemFn elem
            else elem
        List.map f list

let contains letter (xs: Trie<_> list) =
    let f x =
        match x with
        | Leaf (c, _) -> c = letter
        | Node (c, _, _) -> c = letter
    (List.tryFind f xs) <> None

let find (letter: char) (xs: Trie<'T> list) =
    let f x =
        match x with
        | Leaf (c, _) -> c = letter
        | Node (c, _, _) -> c = letter
    (List.tryFind f xs)
// HELPERS

let rec insert (word:char seq) data trie =
    if Seq.isEmpty word then
        raise (ArgumentException "No empty string")

    let initialChar = match trie with
                      | Leaf (c, _) -> c
                      | Node(c, _, _) -> c

    if (Seq.head word) <> initialChar then
        raise (ArgumentException "String must start with the same letter")

    match trie with
    | Leaf (c, existingData) ->
        match Seq.length word with
        | 1 -> trie
        | x -> Node (c, (EOW existingData), [create (Seq.tail word) data])
    | Node(c, wordBoundary, nodeList) ->
        match Seq.length word with
        | 1 -> Node(c, EOW data, nodeList)
        | x ->
            if (contains (word |> Seq.tail |> Seq.head) nodeList) then
                Node (
                        c,
                        wordBoundary,
                        (
                            List.replace (fun n ->
                                match n with
                                | Leaf (c, _) -> c = (word |> Seq.tail |> Seq.head)
                                | Node (c, _, _) -> c = (word |> Seq.tail |> Seq.head)
                            ) (fun n -> insert (Seq.tail word) data n) nodeList
                        )
                    )
            else
                Node (c, wordBoundary, nodeList @ [create (Seq.tail word) data]) // CASE D


let rec exists word trie =
    if Seq.isEmpty word then
        raise (ArgumentException "No empty string")

    match trie with
    | Leaf (c, _) ->
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


let rec findByPrefix (word) (trie): Trie<'T> list option =
    if Seq.isEmpty word then
        raise (ArgumentException "No empty string")

    match trie with
    | Leaf (c, data) ->
        if Seq.head word = c && (Seq.length word = 1) then
            [Leaf (c, data)] |> Some
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
        | Leaf (c, _) -> leafFn (acc + c.ToString())
        | Node (c, wordBoundary, nodeList) ->
            match wordBoundary with
            | EOW _ -> leafFn (acc + c.ToString())
            | _ -> ()
            List.iter (internalFn <| acc + c.ToString()) nodeList

    internalFn "" trie
    results.ToArray() |> Array.toList

let stringPairs (trie:Trie<'T>) =
    let results = new ResizeArray<(string * 'T)>()
    let leafFn tuple =
        results.Add tuple
        ()

    let rec internalFn acc (trie) =
        match trie with
        | Leaf (c, data) -> leafFn (acc + c.ToString(), data)
        | Node (c, wordBoundary, nodeList) ->
            match wordBoundary with
            | EOW data -> leafFn (acc + c.ToString(), data)
            | _ -> ()
            List.iter (internalFn <| acc + c.ToString()) nodeList

    internalFn "" trie
    results.ToArray() |> Array.toList

// TESTS
// create
assertEqual (create<string> "a" "url")      (Leaf ('a', "url"))
assertEqual (create<string> "ab" "url")     (Node ('a', NotEOW, [Leaf ('b', "url")]))

// insert
assertEqual ((create<string> "a" "url") |> (insert "a" "url"))      (Leaf ('a', "url"))

assertEqual (create<string> "ab" "url1" |> insert "a" "url2")       (Node ('a', EOW "url2", [Leaf ('b', "url1")]))

//SHOULD UPDATE
assertEqual (create<string> "ab" "url1" |> insert "ab" "url2")      (Node ('a', NotEOW, [Leaf ('b', "url1")])) 
//SHOULD UPDATE

assertEqual (create<string> "ab" "url1" |> insert "abc" "url2")      (Node ('a', NotEOW, [Node ('b', EOW "url1", [Leaf ('c', "url2")])]))
assertEqual (create<string> "ab" "url1" |> insert "ad" "url2")       (Node ('a', NotEOW, [Leaf ('b', "url1"); Leaf ('d', "url2")]))
assertEqual (create<string> "abc" "url1"|> insert "ad" "url2")       (Node ('a', NotEOW, [Node ('b', NotEOW, [Leaf ('c', "url1")]); Leaf ('d', "url2")]))
assertEqual (create<string> "abc" "url1"|> insert "abd" "url2")      (Node ('a', NotEOW, [Node ('b', NotEOW, [Leaf ('c', "url1"); Leaf ('d', "url2")])]))

// exists
assertEqual (create<string> "a" "url" |> exists "a")                     true
assertEqual (create<string> "a" "url" |> exists "b")                     false
assertEqual (create<string> "a" "url" |> exists "ab")                    false
assertEqual (create<string> "ab" "url" |> exists "a")                    true
assertEqual (create<string> "ab" "url" |> exists "b")                    false
assertEqual (create<string> "ab" "url" |> exists "ab")                   true
assertEqual (create<string> "ab" "url" |> exists "abc")                  false
assertEqual (create<string> "abc" "url" |> insert "abd" "url2" |> exists "ab")  true
assertEqual (create<string> "abc" "url" |> insert "abd" "url2" |> exists "abc") true
assertEqual (create<string> "abc" "url" |> insert "abd" "url2" |> exists "abd") true

// findByPrefix
assertEqual (create<string> "a" "url" |> findByPrefix "a")    (Some [Leaf ('a', "url")])
assertEqual (create<string> "a" "url" |> findByPrefix "ab")   None
assertEqual (create<string> "a" "url" |> findByPrefix "b")    None
assertEqual (create<string> "ab" "url" |> findByPrefix "b")   None
assertEqual (create<string> "abc" "url" |> findByPrefix "b")  None

assertEqual (create<string> "ab" "url"|> findByPrefix "a")                                                       (Some <| [Leaf ('b', "url")])
assertEqual (create<string> "ab" "url1"|> insert "ad" "url2" |> findByPrefix "a")                                (Some <| [Leaf ('b', "url1"); Leaf ('d', "url2")])
assertEqual (create<string> "abc" "url1" |> insert "abd" "url2" |> findByPrefix "ab")                                     (Some <| [Leaf ('c', "url1"); Leaf ('d', "url2")])
assertEqual (
    create<string> "abc" "url1"
    |> insert "abd" "url2"
    |> insert "ace" "url3"
    |> insert "acf" "url4"
    |> findByPrefix "ab"
    )     (Some <| [Leaf ('c', "url1"); Leaf ('d', "url2")])

assertEqual (
    create<string> "abc" "url1"
    |> insert "abd" "url2"
    |> insert "ace" "url3"
    |> insert "acf" "url4"
    |> findByPrefix "ac"
    )     (Some <| [Leaf ('e', "url3"); Leaf ('f', "url4")])

// strings
assertEqual (create<string> "abc" "url" |> strings) ["abc"]
assertEqual (
    create<string> "a" "url1" 
    |> insert "ab" "url2"
    |> insert "abc" "url3"
    |> insert "abd" "url4"
    |> insert "ace" "url5"
    |> insert "acf" "url6"
    |> strings
    ) ["a"; "ab"; "abc"; "abd"; "ace"; "acf";]

assertEqual (
    create<string> "a" "url1" 
    |> insert "ab" "url2"
    |> insert "abc" "url3"
    |> insert "abd" "url4"
    |> insert "ace" "url5"
    |> insert "acf" "url6"
    |> stringPairs
) [("a", "url1"); ("ab", "url2"); ("abc", "url3"); ("abd", "url4"); ("ace", "url5"); ("acf", "url6")]

let dictionary = [("ab", "url2"); ("abc", "url3"); ("ac", "url4"); ("abd", "url5")] in
    let trie = List.fold (fun trie (word, url) -> insert word url trie) (create "a" "url1") dictionary in
        assertEqual (trie |> strings |> List.length) (dictionary.Length + 1)

create "abcdefgh" "url"
|> insert "abd" "url1"
|> insert "ace" "url2"
|> insert "acf" "url3"
|> findByPrefix "ab"
|> Option.get
|> List.collect stringPairs
|> List.map (fun (word, data) -> ("ab" + word , data))
|> l