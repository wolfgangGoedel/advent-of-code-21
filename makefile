aoc:
	@ocamlc aoc.ml

day1: aoc
	@ocaml aoc.cmo day1.ml < day1.txt

day2: aoc
	@ocaml aoc.cmo day2.ml < day2.txt

day3: aoc
	@ocaml aoc.cmo day3.ml < day3.txt

day4: aoc
	@ocaml aoc.cmo day4.ml < day4.txt
