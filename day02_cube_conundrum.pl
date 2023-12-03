% -*- mode: Prolog -*-

/*

--- Day 2: Cube Conundrum ---

You're launched high into the atmosphere! The apex of your trajectory
just barely reaches the surface of a large island floating in the
sky. You gently land in a fluffy pile of leaves. It's quite cold, but
you don't see much snow. An Elf runs over to greet you.

The Elf explains that you've arrived at Snow Island and apologizes for
the lack of snow. He'll be happy to explain the situation, but it's a
bit of a walk, so you have some time. They don't get many visitors up
here; would you like to play a game in the meantime?

As you walk, the Elf shows you a small bag and some cubes which are
either red, green, or blue. Each time you play this game, he will hide
a secret number of cubes of each color in the bag, and your goal is to
figure out information about the number of cubes.

To get information, once a bag has been loaded with cubes, the Elf
will reach into the bag, grab a handful of random cubes, show them to
you, and then put them back in the bag. He'll do this a few times per
game.

You play several games and record the information from each game (your
puzzle input). Each game is listed with its ID number (like the 11 in
Game 11: ...) followed by a semicolon-separated list of subsets of
cubes that were revealed from the bag (like 3 red, 5 green, 4 blue).

For example, the record of a few games might look like this:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

In game 1, three sets of cubes are revealed from the bag (and then put
back again). The first set is 3 blue cubes and 4 red cubes; the second
set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is
only 2 green cubes.

The Elf would first like to know which games would have been possible
if the bag contained only 12 red cubes, 13 green cubes, and 14 blue
cubes?

In the example above, games 1, 2, and 5 would have been possible if
the bag had been loaded with that configuration. However, game 3 would
have been impossible because at one point the Elf showed you 20 red
cubes at once; similarly, game 4 would also have been impossible
because the Elf showed you 15 blue cubes at once. If you add up the
IDs of the games that would have been possible, you get 8.

Determine which games would have been possible if the bag had been
loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. What
is the sum of the IDs of those games?

*/

:- module(day02, [run_tests/0, solve/1]).

:- use_module(library(plunit)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

game_cubes(game{red:12, green:13, blue:14}).

solve(Sum) :-
    read_file_to_lines('data/day02_cube_conundrum.data', Lines),
    game_cubes(Game),
    solve(Game, Lines, Sum).

solve(Game, Lines, Sum) :-
    maplist(parse_game, Lines, Games),
    convlist(possible_game(Game), Games, PossibleGameIds),
    sum_list(PossibleGameIds, Sum).

possible_game(Game, GameId-Grabs, GameId) :-
    maplist(possible_grab(Game), Grabs).

possible_grab(Game, Grab) :-
    dict_pairs(Grab, _, GrabPairs),
    maplist(possible_grab_pair(Game), GrabPairs).

possible_grab_pair(Game, Color-Number) :-
    get_dict(Color, Game, GameColorNumber),
    GameColorNumber >= Number.

parse_game(Line, GameId-Grabs) :-
    string_codes(Line, Codes),
    phrase(parse_game(GameId, Grabs), Codes).

parse_game(GameId, Grabs) -->
    whites,
    "Game", whites,
    integer(GameId),
    ":", whites,
    sequence(grab, semicolon, Grabs).

semicolon --> ";", whites.

grab(GrabColors) -->
    sequence(grab_color, ",", GrabColors0),
    { dict_create(GrabColors, grab, GrabColors0) }.

grab_color(Color-Number) -->
    whites,
    integer(Number),
    " ", whites,
    color(Color).

color(red) --> "red".
color(green) --> "green".
color(blue) --> "blue".

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

:- begin_tests(day2).

test(parse, G == 1) :-
    parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", GV),
    assertion(GV == 1-[grab{blue:3, red:4}, grab{red:1, green:2, blue:6}, grab{green:2}]),
    game_cubes(Game),
    possible_game(Game, GV, G).
test(parse, G == 2) :-
    parse_game("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", GV),
    assertion(GV == 2-[grab{blue:1, green:2}, grab{green:3, blue:4, red:1}, grab{green:1, blue:1}]),
    game_cubes(Game),
    possible_game(Game, GV, G).
test(parse) :-
    parse_game("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", GV),
    assertion(GV == 3-[grab{green:8, blue:6, red:20}, grab{blue:5, red:4, green:13}, grab{green:5, red:1}]),
    game_cubes(Game),
    assertion(\+ possible_game(Game, GV, _)).
test(parse) :-
    parse_game("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", GV),
    assertion(GV == 4-[grab{green:1, red:3, blue:6}, grab{green:3, red:6}, grab{green:3, blue:15, red:14}]),
    game_cubes(Game),
    assertion(\+ possible_game(Game, GV, _)).
test(parse, G == 5) :-
    parse_game("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", GV),
    assertion(GV == 5-[grab{red:6, blue:1, green:3}, grab{blue:2, red:1, green:2}]),
    game_cubes(Game),
    possible_game(Game, GV, G).

test(solve, Sum == 8) :-
    open_string("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
", Stream),
    read_stream_to_lines(Stream, Lines),
    solve(game{red:12, green:13, blue:14}, Lines, Sum).

:- end_tests(day2).
