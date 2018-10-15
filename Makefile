.PHONY: test

clean:
	rm -rf _build
	find . -name .merlin | xargs rm

deps: opam-deps dune-deps

# - tell opam about this package
# - install all non-test dependencies
# - install test dependencies
#
# (Note: opam install --deps-only -t would also
# install test dependencies, but it would install them
# recursively for all other dependencies, which isn't
# really what we want)
opam-deps:
	opam pin add -yn tnqcc .
	opam install --deps-only tnqcc -y
	opam install ounit


dune-deps:
	dune external-lib-deps --missing src/.utop/utop.exe
	dune external-lib-deps --missing @runtest

test:
	dune build @runtest -w

utop:
	dune utop src

build:
	dune build app/cli.exe

# Can run with dune run app/cli.exe -- ARGS
