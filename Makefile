.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop bin

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f rml.zip
	zip -r rml.zip . -x@exclude.lst

clean:
	dune clean
	rm -f rml.zip

doc:
	dune build @doc
