.PHONY: build
build:
	dune build

# Rebuild the library and the fuzz harness (fuzz.exe) with AFL instrumentation,
# then run the fuzzer (afl-fuzz).
#
# Command to install an opam switch with the compiler set up to produce
# AFL-instrumented executables:
# $ opam switch create afl-4.14.3 --packages=ocaml-variants.4.14.3+options,ocaml-option-afl
#
# See 'dune exec -- ./fuzz.exe --help' for options.
.PHONY: fuzz
fuzz:
	dune build fuzz.exe
	./run-afl 1
