# What is this?
This is a compiler for a subset Pascal written in Haskell.  
The grammar and semantics largely follow [this book](https://www.amazon.co.jp/%E3%82%B3%E3%83%B3%E3%83%91%E3%82%A4%E3%83%A9-%E7%AC%AC2%E7%89%88-%E8%BE%BB%E9%87%8E-%E5%98%89%E5%AE%8F/dp/4274224724).

## Usage
[Stack](https://docs.haskellstack.org/en/stable/) (a Haskell build tool) is required.  

```
$ stack build
$ stack test
$ stack test --test-argumant "-m Parser"  // testing only for the parser
```