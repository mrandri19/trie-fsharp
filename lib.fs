namespace AndreaCognolato

module Trie =

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
        | Node of char * Trie list

    let rec create (word:char seq): Trie =
        match Seq.length word with
        | 0 -> raise (ArgumentException "No empty string")
        | 1 -> Leaf (Seq.head word)
        | x -> Node (Seq.head word, [create (Seq.tail word)])

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
            | Node (c,_) -> c = letter
        (List.tryFind f xs) <> None

    let find (letter: char) (xs: Trie list) =
        let f x =
            match x with
            | Leaf c -> c = letter
            | Node (c,_) -> c = letter
        (List.tryFind f xs)
    // HELPERS

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
        | Node(c, nodeList) ->
            match Seq.length word with
            | 1 -> trie
            | x ->
                if (contains (word |> Seq.tail |> Seq.head) nodeList) then
                    Node (
                            c,
                            (
                                List.replace (fun n ->
                                    match n with
                                    | Leaf c -> c = (word |> Seq.tail |> Seq.head)
                                    | Node (c,_) -> c = (word |> Seq.tail |> Seq.head)
                                ) (fun n -> insert (Seq.tail word) n) nodeList
                            )
                        )
                else
                    Node (c, nodeList @ [create (Seq.tail word)]) // CASE D


    let rec exists word trie =
        if Seq.isEmpty word then
            raise (ArgumentException "No empty string")

        match trie with
        | Leaf c ->
            if Seq.length word = 1 then
                Seq.head word = c
            else false
        | Node (c, nodeList) ->
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
            None
        | Node (c, nodeList) ->
            if Seq.head word = c then
                match Seq.length word with
                | 1 -> Some nodeList
                | x ->
                    match (find (word |> Seq.tail |> Seq.head) nodeList) with
                    | None -> None
                    | Some n -> findByPrefix (Seq.tail word) n
            else None


    let strings trie =
        let mutable results = [||]
        let leafFn s =
            results <- Array.append [|s|] results
            ()

        let rec internalFn acc (trie) =
            match trie with
            | Leaf c -> leafFn (acc + c.ToString())
            | Node (c,nodeList) ->
                List.iter (internalFn <| acc + c.ToString()) nodeList

        internalFn "" trie
        results |> Array.toList
