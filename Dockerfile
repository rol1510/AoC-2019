FROM ocaml/opam:ubuntu-23.04-ocaml-5.1
RUN sudo apt-get update
RUN sudo apt-get upgrade -y
RUN sudo apt-get install rlwrap -y
WORKDIR /aoc
