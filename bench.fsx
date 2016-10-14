#r @"lib.dll"

open AndreaCognolato.trie

open System
let newline() = printfn "" 
let words =  IO.File.ReadAllLines @"../crawler_latino/collatedWords.txt"

printfn "Prepend '/' to all words we'll insert so they can all start with a different letter"
let finalwords = Array.map ((+) "/") words


printfn "Fill the trie with 68k words"
#time
let fullTrie = Array.fold (fun trie word -> insert word trie) (create "/") finalwords
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

printfn """List.map strings (findByPrefix "/")"""
let t1 = findByPrefix "/" fullTrie |> Option.get
#time
List.map strings t1
#time

newline()

printfn """List.map strings (findByPrefix "/a")"""
let t2 = findByPrefix "/a" fullTrie |> Option.get
#time
List.map strings t2
#time

newline()

printfn """List.map strings (findByPrefix "/ab")"""
let t3 = findByPrefix "/ab" fullTrie |> Option.get
#time
List.map strings t3
#time