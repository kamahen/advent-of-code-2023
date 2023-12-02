% -*- mode: Prolog -*-

/*
https://adventofcode.com/2023/day/1

--- Day 1: Trebuchet?! ---

Something is wrong with global snow production, and you've been
selected to take a look. The Elves have even given you a map; on it,
they've used stars to mark the top fifty locations that are likely to
be having problems.

You've been doing this long enough to know that to restore snow
operations, you need to check all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available
on each day in the Advent calendar; the second puzzle is unlocked when
you complete the first. Each puzzle grants one star. Good luck!

You try to ask why they can't just use a weather machine ("not
powerful enough") and where they're even sending you ("the sky") and
why your map looks mostly blank ("you sure ask a lot of questions")
and hang on did you just say the sky ("of course, where do you think
snow comes from") when you realize that the Elves are already loading
you into a trebuchet ("please hold still, we need to strap you in").

As they're making the final adjustments, they discover that their
calibration document (your puzzle input) has been amended by a very
young Elf who was apparently just excited to show off her art
skills. Consequently, the Elves are having trouble reading the values
on the document.

The newly-improved calibration document consists of lines of text;
each line originally contained a specific calibration value that the
Elves now need to recover. On each line, the calibration value can be
found by combining the first digit and the last digit (in that order)
to form a single two-digit number.

For example:

1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet

In this example, the calibration values of these four lines are 12,
38, 15, and 77. Adding these together produces 142.

Consider your entire calibration document. What is the sum of all of
the calibration values?

Your puzzle answer was 56049.

--- Part Two ---

Your calculation isn't quite right. It looks like some of the digits
are actually spelled out with letters: one, two, three, four, five,
six, seven, eight, and nine also count as valid "digits".

Equipped with this new information, you now need to find the real
first and last digit on each line. For example:

two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen

In this example, the calibration values are 29, 83, 13, 24, 42, 14,
and 76. Adding these together produces 281.

What is the sum of all of the calibration values?

Your puzzle answer was 54530.
*/

:- module(day01, [run_tests/0, solve/1]).

:- use_module(library(plunit)).

solve(Sum) :-
    read_file_to_lines('data/day01_trebuchet.data', Lines),
    solve(Lines, Sum).

solve(Lines, Sum) :-
    maplist(calibration_value, Lines, Values),
    sum_list(Values, Sum).

%! calibration_value(+Str:string, -Value:int) is det.
calibration_value(Str, Value) :-
    string_codes(Str, Codes),
    phrase(first_digit(fwd, First), Codes),
    reverse(Codes, CodesR),
    phrase(first_digit(rev, Last), CodesR),
    Value is First * 10 + Last.

first_digit(Dir, First) -->
    (   digit(Dir, First)
    ->  skip_rest
    ;   [_],
        first_digit(Dir, First)
    ).

digit(fwd, 0) --> "zero".
digit(fwd, 1) --> "one".
digit(fwd, 2) --> "two".
digit(fwd, 3) --> "three".
digit(fwd, 4) --> "four".
digit(fwd, 5) --> "five".
digit(fwd, 6) --> "six".
digit(fwd, 7) --> "seven".
digit(fwd, 8) --> "eight".
digit(fwd, 9) --> "nine".
digit(rev, 0) --> "orez".
digit(rev, 1) --> "eno".
digit(rev, 2) --> "owt".
digit(rev, 3) --> "eerht".
digit(rev, 4) --> "ruof".
digit(rev, 5) --> "evif".
digit(rev, 6) --> "xis".
digit(rev, 7) --> "neves".
digit(rev, 8) --> "thgie".
digit(rev, 9) --> "enin".
digit(_, 0) --> "0".
digit(_, 1) --> "1".
digit(_, 2) --> "2".
digit(_, 3) --> "3".
digit(_, 4) --> "4".
digit(_, 5) --> "5".
digit(_, 6) --> "6".
digit(_, 7) --> "7".
digit(_, 8) --> "8".
digit(_, 9) --> "9".

skip_rest --> [].
skip_rest --> [_], skip_rest.

read_file_to_lines(Path, Lines) :-
    setup_call_cleanup(open(Path, read, Stream),
                       ( read_stream_to_lines(Stream, Lines0),
                         exclude(=(""), Lines0, Lines) ),
                       close(Stream)).

read_stream_to_lines(Stream, Lines) :-
    read_line_to_string(Stream, Line),
    read_stream_to_lines(Stream, Line, Lines).

read_stream_to_lines(_, end_of_file, []) :- !.
read_stream_to_lines(Stream, Line, [Line|Lines2]) :-
    read_stream_to_lines(Stream, Lines2).

:- begin_tests(day1).

test(calibration_value, V == 12) :-
    calibration_value("1abc2", V).
test(calibration_value, V == 38) :-
    calibration_value("pqr3stu8vwx", V).
test(calibration_value, V == 15) :-
    calibration_value("a1b2c3d4e5f", V).
test(calibration_value, V == 77) :-
    calibration_value("treb7uchet", V).

test(calibration_value, V == 29) :-
    calibration_value("two1nine", V).
test(calibration_value, V == 83) :-
    calibration_value("eightwothree", V).
test(calibration_value, V == 13) :-
    calibration_value("abcone2threexyz", V).
test(calibration_value, V == 24) :-
    calibration_value("xtwone3four", V).
test(calibration_value, V == 42) :-
    calibration_value("4nineeightseven2", V).
test(calibration_value, V == 14) :-
    calibration_value("zoneight234", V).
test(calibration_value, V == 76) :-
    calibration_value("7pqrstsixteen", V).

test(reverse, V == 00) :-
    calibration_value("zero", V).
test(reverse, V == 11) :-
    calibration_value("one", V).
test(reverse, V == 22) :-
    calibration_value("two", V).
test(reverse, V == 33) :-
    calibration_value("three", V).
test(reverse, V == 44) :-
    calibration_value("four", V).
test(reverse, V == 55) :-
    calibration_value("five", V).
test(reverse, V == 66) :-
    calibration_value("six", V).
test(reverse, V == 77) :-
    calibration_value("seven", V).
test(reverse, V == 88) :-
    calibration_value("eight", V).
test(reverse, V == 99) :-
    calibration_value("nine", V).

test(solve, Sum == 142) :-
    open_string("1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet", Stream),
    read_stream_to_lines(Stream, Lines),
    solve(Lines, Sum).

:- end_tests(day1).
