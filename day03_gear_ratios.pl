% -*- mode: Prolog -*-

/*

--- Day 3: Gear Ratios ---

You and the Elf eventually reach a gondola lift station; he says the
gondola lift will take you up to the water source, but this is as far
as he can bring you. You go inside.

It doesn't take long to find the gondolas, but there seems to be a
problem: they're not moving.

"Aaah!"

You turn around to see a slightly-greasy Elf with a wrench and a look
of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't
working right now; it'll still be a while before I can fix it." You
offer to help.

The engineer explains that an engine part seems to be missing from the
engine, but nobody can figure out which one. If you can add up all the
part numbers in the engine schematic, it should be easy to work out
which part is missing.

The engine schematic (your puzzle input) consists of a visual
representation of the engine. There are lots of numbers and symbols
you don't really understand, but apparently any number adjacent to a
symbol, even diagonally, is a "part number" and should be included in
your sum. (Periods (.) do not count as a symbol.)

Here is an example engine schematic:

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

In this schematic, two numbers are not part numbers because they are
not adjacent to a symbol: 114 (top right) and 58 (middle right). Every
other number is adjacent to a symbol and so is a part number; their
sum is 4361.

Of course, the actual engine schematic is much larger. What is the sum
of all of the part numbers in the engine schematic?

*/

:- module(day03, [run_tests/0, solve/1]).

:- use_module(library(plunit)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

solve(_) :- fail.

cell(Parsed, I, J, V) :-
    nth0(I, Parsed, V0),
    nth0(J, V0, V).

parse_diagram(Stream, Codes) :-
    read_stream_to_codes(Stream, Codes0),
    phrase(parse_diagram(Codes1), Codes0),
    !, % string//0 leaves a choicepoint
    exclude(=([]), Codes1, Codes).

parse_diagram(Codes) -->
    sequence(string, nl, Codes).

nl --> "\r\n", !.
nl --> "\n\r", !.
nl --> "\n", !.
nl --> "\r", !.

:- begin_tests(day3).

test(parse) :-
    open_string("0123
4567
abcd
efgh", Stream),
    parse_diagram(Stream, Parsed),
    assertion(Parsed == [[0'0,0'1,0'2,0'3],
                         [0'4,0'5,0'6,0'7],
                         [0'a,0'b,0'c,0'd],
                         [0'e,0'f,0'g,0'h]]),
    assertion(cell(Parsed, 0, 0, 0'0)),
    assertion(cell(Parsed, 0, 1, 0'1)),
    assertion(cell(Parsed, 0, 2, 0'2)),
    assertion(cell(Parsed, 0, 3, 0'3)),
    assertion(cell(Parsed, 1, 0, 0'4)),
    assertion(cell(Parsed, 1, 1, 0'5)),
    assertion(cell(Parsed, 1, 2, 0'6)),
    assertion(cell(Parsed, 1, 3, 0'7)),
    assertion(cell(Parsed, 2, 0, 0'a)),
    assertion(cell(Parsed, 2, 1, 0'b)),
    assertion(cell(Parsed, 2, 2, 0'c)),
    assertion(cell(Parsed, 2, 3, 0'd)),
    assertion(cell(Parsed, 3, 0, 0'e)),
    assertion(cell(Parsed, 3, 1, 0'f)),
    assertion(cell(Parsed, 3, 2, 0'g)),
    assertion(cell(Parsed, 3, 3, 0'h)).
test(parse) :-
    open_string("467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..", Stream),
    parse_diagram(Stream, Parsed),
    assertion(Parsed == [[0'4,0'6,0'7,0'.,0'.,0'1,0'1,0'4,0'.,0'.],
                         [0'.,0'.,0'.,0'*,0'.,0'.,0'.,0'.,0'.,0'.],
                         [0'.,0'.,0'3,0'5,0'.,0'.,0'6,0'3,0'3,0'.],
                         [0'.,0'.,0'.,0'.,0'.,0'.,0'#,0'.,0'.,0'.],
                         [0'6,0'1,0'7,0'*,0'.,0'.,0'.,0'.,0'.,0'.],
                         [0'.,0'.,0'.,0'.,0'.,0'+,0'.,0'5,0'8,0'.],
                         [0'.,0'.,0'5,0'9,0'2,0'.,0'.,0'.,0'.,0'.],
                         [0'.,0'.,0'.,0'.,0'.,0'.,0'7,0'5,0'5,0'.],
                         [0'.,0'.,0'.,0'$,0'.,0'*,0'.,0'.,0'.,0'.],
                         [0'.,0'6,0'6,0'4,0'.,0'5,0'9,0'8,0'.,0'.]]).

:- end_tests(day3).
