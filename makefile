aoc: aoc.ml
	@ocamlc aoc.ml

day1: aoc
	@ocaml aoc.cmo day1.ml < day1.txt
