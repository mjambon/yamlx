# Entry points for manage the project

# Build everything
.PHONY: build
build:
	opam exec -- dune build

.PHONY: test
test:
	opam exec -- dune runtest

# Create a local opam switch (= install deps only for this project)
.PHONY: setup-opam
setup-opam:
	opam switch create . ocaml-base-compiler.5.3.0 --deps-only -y
	$(MAKE) install-deps

.PHONY: install-deps
install-deps:
	opam install ./yamlx.opam -y --deps-only --with-test --with-doc

# Build odoc HTML documentation and copy it into docs/ (served by GitHub Pages)
.PHONY: doc
doc:
	opam exec -- dune build @doc
	rm -rf docs/
	cp -a _build/default/_doc/_html/. docs/

# Fail if running 'make doc' would change any file under docs/.
# Used by the pre-commit hook and CI to enforce that committed docs are current.
.PHONY: doc-check
doc-check:
	$(MAKE) doc
	git diff --exit-code docs/

# Remove build products. Keep '_opam/'.
.PHONY: clean
clean:
	rm -rf _build

# Remove any file that's gitignored.
.PHONY: distclean
distclean:
	 git clean -dfX
