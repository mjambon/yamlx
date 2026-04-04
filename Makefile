# Entry points for manage the project

# Build everything
.PHONY: build
build:
	opam exec -- dune build

.PHONY: test
test:
	opam exec -- dune build tests/Test_yamlx.exe
	opam exec -- ./test

# Create a local opam switch (= install deps only for this project)
.PHONY: setup-opam
setup-opam:
	opam switch create . ocaml-base-compiler.5.3.0 --deps-only -y
	$(MAKE) install-deps

.PHONY: install-deps
install-deps:
	opam install ./yamlx.opam -y --deps-only --with-test --with-doc

# Remove build products. Keep '_opam/'.
.PHONY: clean
clean:
	rm -rf _build

# Remove any file that's gitignored.
.PHONY: distclean
distclean:
	 git clean -dfX
