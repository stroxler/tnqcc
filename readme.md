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

# Running things

## Setup

First install opam and the build dependencies. On OSX, you
can do
```
brew install opam fswatch
opam install -y dune merline ocp-indent utop
```

Next, pin this package and install dependencies. There's
already a command for this in the makefile, so you can just run:
```
make opam-deps
```

## Running: interactive, unit tests, creating/running executable

Now, you can get an interactive shell to play with the code via
```
make utop
```
run the tests in a loop that watches the code via
```
make test
```
and build the executable via
```
make build
```

You can run the executable (which has subcommands `lex`, `parse`, and `gen`)
by running
```
./_build/app/cli.exe [COMMAND] [ARGS]
```
(and using `--help` to discover the args).

## The tnqcc executable

There's a script, `./tnqcc`, that wraps the `gen` functionality in a shell
script which takes the path to a `*.c` file as it's only argument, generates
a `*.s` file, and calls `gcc` to produce a binary. This script is compatible
with Nora Sandler's
[write_a_c_compiler](https://github.com/nlsandler/write_a_c_compiler)
compiler integration test suite.

### Running the integ test suite

If you either clone the `write_a_c_compiler` code into an adjacent directory -
e.g. by running
```
cd ..
git clone https://github.com/nlsandler/write_a_c_compiler
```
- or clone it into an arbitrary directory and set the `WACC_DIR` environment
variable then you can run the test suite by running
```
./run_wacc_tests.sh
```

I modified both the `write_a_c_compiler` script and my own script to take
as arguments the number of steps to run; as of this commit only the first
step is working. I made a PR to upstream; for now you can use the `num-stages`
branch of `stroxler/write_a_c_compiler` if you want to test just one stage.


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
gcc -m32 -S -O0 -fno-asynchronous-unwind-tables <file.c>
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

