# Entry points for manage the project

# Build everything
.PHONY: build
build:
	opam exec -- dune build

.PHONY: test
test:
	opam exec -- dune runtest -f

# Generate random YAML input and try parsing it and printing it until
# something bad happens.
.PHONY: fuzz
fuzz:
	$(MAKE) -C fuzz fuzz

# Run benchmarks against a large YAML file.
# Suggested test file: https://github.com/aaubry/YamlDotNet/issues/519
.PHONY: bench
bench:
	opam exec -- dune exec benchmarks/main.exe -- benchmarks/saltern.yml

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

# ------------------------------------------------------------------ #
# Release process                                                      #
# ------------------------------------------------------------------ #
#
# Prerequisites: dune-release must be installed.
#   opam install dune-release
#
# Steps:
#   1. Run 'make opam-files' to regenerate the opam files from dune-project.
#   2. Review and update CHANGES.md: add a section heading with the version
#      and date, e.g. "## 0.2.0 (2026-05-01)".
#   3. Commit the changes on the main branch.
#   4. Run 'dune-release tag'   — picks the version from CHANGES.md and tags
#                                  the commit; asks for confirmation.
#   5. Run 'dune-release distrib' — builds and checks the release tarball.
#   6. Run 'dune-release publish' — uploads the tarball to GitHub and creates
#                                   the GitHub release (point of no return).
#   7. Run 'dune-release opam pkg' — prepares the opam package description.
#   8. Run 'dune-release opam submit' — opens a PR on opam-repository.
#   9. Monitor the opam-repository PR; fix CI failures and address reviewer
#      feedback until the PR is merged.
#
# Steps 4–8 can be run in one shot with 'make opam-release'.
# If anything fails after step 6, increment the version and start over.
#
.PHONY: opam-files
opam-files:
	opam exec -- dune build yamlx.opam

.PHONY: opam-release
opam-release:
	dune-release tag
	dune-release distrib
	dune-release publish
	dune-release opam pkg
	dune-release opam submit
