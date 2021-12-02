aoc:
	@ocamlc aoc.ml

day1: aoc
	@ocaml aoc.cmo day1.ml < day1.txt

day2: aoc
	@ocaml aoc.cmo day2.ml < day2.txt
