# Run

If you don't want to install ocaml, you can use the docker container.

	docker build -t my-ocaml .
	docker run -it -v <path-to-repo>:/aoc/ my-ocaml

To run the files use:

	ocamlc str.cma -I +str <file-name> && ./a.out

or

	run_all.sh
