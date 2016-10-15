# Trie F#
An implementation of a [trie](https://en.wikipedia.org/wiki/Trie) in f#, it's quite simple and **NOT** intended for production use.

Currently supports only words starting with the same letter. `:(`
## Methods
- `create`
- `insert`
- `exists`
- `findByPrefix`
- `strings`

## Benchmarks
These were performed with the file `bench.fsx` using and Intel i3570k CPU and 8GB 1660MHz on `Linux 4.7.6-1-ARCH x86_64`
```
Prepend '/' to all words we'll insert so they can all start with a different letter
Fill the trie with 68k words
Real: 00:00:02.783, CPU: 00:00:02.770, GC gen0: 348, gen1: 0

Use `findByPrefix "/abi" fullTrie` 10^5 times
Real: 00:00:00.316, CPU: 00:00:00.316, GC gen0: 56, gen1: 0

Use `findByPrefix "/" fullTrie` 10^5 times
Real: 00:00:00.013, CPU: 00:00:00.016, GC gen0: 2, gen1: 0

List.map strings (findByPrefix "/") 10^1 times
Real: 00:00:00.618, CPU: 00:00:00.626, GC gen0: 59, gen1: 2

List.map strings (findByPrefix "/a") 10^2 times
Real: 00:00:00.624, CPU: 00:00:00.633, GC gen0: 63, gen1: 2

List.map strings (findByPrefix "/ab") 10^3 times
Real: 00:00:00.261, CPU: 00:00:00.260, GC gen0: 35, gen1: 0
```
