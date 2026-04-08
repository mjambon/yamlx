# Compare the performance of YAMLx against ocaml-yaml which uses
# the C library libyaml.
#
# A large file named target.yaml is expected.
.PHONY: run
run:
	dune build
	dune exec -- ./yamlx_benchmarks.exe target.yaml
