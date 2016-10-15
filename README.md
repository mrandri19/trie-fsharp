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
Real: 00:00:02.790, CPU: 00:00:02.790, GC gen0: 348, gen1: 0

Use `findByPrefix "/abi" fullTrie` 10^5 times
Real: 00:00:00.321, CPU: 00:00:00.323, GC gen0: 56, gen1: 0

Use `findByPrefix "/" fullTrie` 10^5 times
Real: 00:00:00.014, CPU: 00:00:00.016, GC gen0: 2, gen1: 0

List.map strings (findByPrefix "/")
Real: 00:00:02.090, CPU: 00:00:02.300, GC gen0: 18, gen1: 51

List.map strings (findByPrefix "/a")
Real: 00:00:00.017, CPU: 00:00:00.016, GC gen0: 4, gen1: 0

List.map strings (findByPrefix "/ab")
Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0
```
