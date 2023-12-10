.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop bin

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe
	
zip:
	rm -f PortfolioManager.zip
	zip -r PortfolioManager.zip . -x@exclude.lst || echo "No files excluded."

clean:
	dune clean
	rm -f rml.zip

doc:
	dune build @doc
