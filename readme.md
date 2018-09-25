# Tnqcc

My code as I go through
[Nora Sandler's blog series](https://norasandler.com/)
on writing a simple C compiler (for a subset of C).

My code is only half-original; I'm leaning heavily on
Sandler's [nqcc](https://github.com/nlsandler/nqcc)
ocaml implementation, since I'm learning ocaml in the
process.

I'm borrowing a lot of implementation details shamelessly,
but there are a few differences between my code and
Sandler's
 - With the benefit of reading through her final code,
   I'm able to predict the final code structure even
   as I do early steps
 - I'm useing Jane Street `Base` instead of `Batteries`
   as my standard library
 - 

# General commentary

One thing worthy of note is that we are writing a
traditional lexer/parser by hand in this project,
rather than following one of the two typical paths
for ocaml projects that require parsing, which are to
use either:
  - generator libraries (ocamllex, ocamlyacc, menhir, ...)
    to generate all the boilerplate for us, or
  - use a haskell-style parser combinator monad, which
    can do lexing and parsing together with a pretty
    intuitive interface (assuming we understand monads).

I'm hoping to follow up with some variants of this
lexer/parser later using generators and maybe doing
a haskell version with Parsec, but it actually does
seem instructive to try lexing and parsing C by hand
once, and we avoid some more complicated interactions
with the `dune` build system by sticking to vanilla
`.ml` files.

# Gcc nonsense

To compile a simple example, when figuring out what
assembly language to generate, use this command:
```
gcc -m32 -S -O3 -fno-asynchronous-unwind-tables <file.c>
```
This will generate a `<file>.s` file of gcc-style 32 bit
assembly. Note that there's an altenative syntax, nasm,
which is equivalent but has different formatting rules
and reverses the order of argumetents.

To compile a `<file>.s` assembly file, use
```
gcc -m32 <file>.s -o <exe>
```
and then you can run it with `./<exe>`.

(You can omit the `-m32` option in these commands if you want to use 64-bit
assembly, but I was following along with Sandler's code, which is 32-bit - as
are most currently available compiler and assembly language resources).
