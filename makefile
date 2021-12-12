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

day5: aoc
	@ocaml str.cma aoc.cmo day5.ml < day5.txt

day6: aoc
	@ocaml day6.ml < day6.txt

day7: aoc
	@ocaml day7.ml < day7.txt

day8: aoc
	@ocaml aoc.cmo day8.ml < day8.txt
