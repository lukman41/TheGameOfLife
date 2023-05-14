build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f fourLifers.zip
	zip -r fourLifers.zip . -x@exclude.lst

clean:
	dune clean
	rm -f fourLifers.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh