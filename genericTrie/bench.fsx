#r "genericTrie.dll"

open AndreaCognolato.Trie

open System

let l a = printfn "%A" a
let newline() = printfn ""

let words =  IO.File.ReadAllLines @"collatedWordsWithLink.txt"

let wordLinkTuples = Array.map (fun (word:string) ->
                                    let a = word.Split([|' '|])
                                    ("/" + a.[0], a.[1])
                                ) words

printfn "Fill the trie with 68k words"
#time
let fullTrie = Array.fold (fun trie (word, data) -> insert word data trie) (create "/" "") wordLinkTuples
#time

newline()

printfn "Use `findByPrefix \"/abi\" fullTrie` 10^5 times"
#time
for i in 1..(pown 10 5) do
    findByPrefix "/abi" fullTrie |> ignore
#time

newline()

printfn """Use `findByPrefix "/" fullTrie` 10^5 times"""
#time
for i in 1..(pown 10 5) do
    findByPrefix "/" fullTrie |> ignore
#time

newline()

printfn """List.map strings (findByPrefix "/") 10^1 times"""
let t1 = findByPrefix "/" fullTrie |> Option.get
#time
for i in 1..(pown 10 1) do
    List.map strings t1 |> ignore
#time

newline()

printfn """List.map strings (findByPrefix "/a") 10^2 times"""
let t2 = findByPrefix "/a" fullTrie |> Option.get
#time
for i in 1..(pown 10 2) do
    List.map strings t2 |> ignore
#time

newline()

printfn """List.map strings (findByPrefix "/ab") 10^3 times"""
let t3 = findByPrefix "/ab" fullTrie |> Option.get
#time
for i in 1..(pown 10 3) do
    List.map strings t3 |> ignore
#time

newline()

printfn """List.map strings (findByPrefix "/ab") 10^3 times"""
let t4 = findByPrefix "/ab" fullTrie |> Option.get
#time
for i in 1..(pown 10 3) do
    List.map stringPairs t4 |> ignore
#time
