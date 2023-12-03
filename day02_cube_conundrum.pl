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

--- Part Two ---

The Elf says they've stopped producing snow because they aren't
getting any water! He isn't sure why the water stopped; however, he
can show you how to get to the water source to check it out for
yourself. It's just up ahead!

As you continue your walk, the Elf poses a second question: in each
game you played, what is the fewest number of cubes of each color that
could have been in the bag to make the game possible?

Again consider the example games from earlier:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes. If any color had even one fewer cube, the game would have been impossible.
Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
Game 4 required at least 14 red, 3 green, and 15 blue cubes.
Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.

The power of a set of cubes is equal to the numbers of red, green, and
blue cubes multiplied together. The power of the minimum set of cubes
in game 1 is 48. In games 2-5 it was 12, 1560, 630, and 36,
respectively. Adding up these five powers produces the sum 2286.

For each game, find the minimum set of cubes that must have been
present. What is the sum of the power of these sets?

*/

:- module(day02, [run_tests/0, solve/2]).

:- use_module(library(plunit)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

game_cubes(game{red:12, green:13, blue:14}).

solve(Sum, PowerSum) :-
    read_file_to_lines('data/day02_cube_conundrum.data', Lines),
    game_cubes(Game),
    solve(Game, Lines, Sum, PowerSum).

solve(Game, Lines, Sum, PowerSum) :-
    maplist(parse_game, Lines, Games),
    convlist(possible_game(Game), Games, PossibleGameIds),
    sum_list(PossibleGameIds, Sum),
    maplist(minimal_game, Games, MinimalGames),
    maplist(power_of_cubes, MinimalGames, Powers),
    sum_list(Powers, PowerSum).

possible_game(Game, GameId-Grabs, GameId) :-
    maplist(possible_grab(Game), Grabs).

possible_grab(Game, Grab) :-
    dict_pairs(Grab, _, GrabPairs),
    maplist(possible_grab_pair(Game), GrabPairs).

possible_grab_pair(Game, Color-Number) :-
    number_of_cubes(Game, Color, GameColorNumber),
    GameColorNumber >= Number.

power_of_cubes(Game, Power) :-
    number_of_cubes(Game, red, Red),
    number_of_cubes(Game, green, Green),
    number_of_cubes(Game, blue, Blue),
    Power is Red * Green * Blue.

number_of_cubes(Game, Color, Number) :-
    (   get_dict(Color, Game, Number)
    ->  true
    ;   Number = 0
    ).

minimal_game(_GameId-Grabs, Game) :-
    foldl(max_of_grab, Grabs, game{}, Game).

max_of_grab(Grab, Game0, Game) :-
    dict_pairs(Grab, _, GrabPairs),
    foldl(max_of_grab_pair, GrabPairs, Game0, Game).

max_of_grab_pair(Color-Number, Game0, Game) :-
    (   number_of_cubes(Game0, Color, Game0Number),
        Game0Number >= Number
    ->  Game = Game0
    ;   put_dict(Color, Game0, Number, Game)
    ).

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

test_possible_game(GV, G, Possible) :-
    game_cubes(Game),
    test_possible_game(Game, GV, G, Possible).

test_possible_game(Game, GV, G, Possible) :-
    (   possible_game(Game, GV, G)
    ->  Possible = true
    ;   Possible = false
    ).

test(parse, GameId-Possible == 1-true) :-
    parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", GameIdGrabs),
    Grabs = [grab{blue:3, red:4}, grab{red:1, green:2, blue:6}, grab{green:2}],
    assertion(GameIdGrabs == 1-Grabs),
    test_possible_game(GameIdGrabs, GameId, Possible),
    minimal_game(GameIdGrabs, MinimalGame),
    assertion(MinimalGame == game{red:4, green:2, blue:6}),
    power_of_cubes(MinimalGame, Power),
    assertion(Power == 48).
test(parse, GameId-Possible == 2-true) :-
    parse_game("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", GameIdGrabs),
    Grabs = [grab{blue:1, green:2}, grab{green:3, blue:4, red:1}, grab{green:1, blue:1}],
    assertion(GameIdGrabs == 2-Grabs),
    test_possible_game(GameIdGrabs, GameId, Possible),
    minimal_game(GameIdGrabs, MinimalGame),
    assertion(MinimalGame == game{red:1, green:3, blue:4}),
    power_of_cubes(MinimalGame, Power),
    assertion(Power == 12).
test(parse, Possible == false) :-
    parse_game("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", GameIdGrabs),
    Grabs = [grab{green:8, blue:6, red:20}, grab{blue:5, red:4, green:13}, grab{green:5, red:1}],
    assertion(GameIdGrabs == 3-Grabs),
    test_possible_game(GameIdGrabs, _, Possible),
    minimal_game(GameIdGrabs, MinimalGame),
    assertion(MinimalGame == game{red:20, green:13, blue:6}),
    power_of_cubes(MinimalGame, Power),
    assertion(Power == 1560).
test(parse, Possible == false) :-
    parse_game("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", GameIdGrabs),
    Grabs = [grab{green:1, red:3, blue:6}, grab{green:3, red:6}, grab{green:3, blue:15, red:14}],
    assertion(GameIdGrabs == 4-Grabs),
    test_possible_game(GameIdGrabs, _, Possible),
    minimal_game(GameIdGrabs, MinimalGame),
    assertion(MinimalGame == game{red:14, green:3, blue:15}),
    power_of_cubes(MinimalGame, Power),
    assertion(Power == 630).
test(parse, GameId-Possible == 5-true) :-
    parse_game("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", GameIdGrabs),
    Grabs = [grab{red:6, blue:1, green:3}, grab{blue:2, red:1, green:2}],
    assertion(GameIdGrabs == 5-Grabs),
    test_possible_game(GameIdGrabs, GameId, Possible),
    minimal_game(GameIdGrabs, MinimalGame),
    assertion(MinimalGame == game{red:6, green:3, blue:2}),
    power_of_cubes(MinimalGame, Power),
    assertion(Power == 36).

test(solve, Sum-PowerSum == 8-2286) :-
    open_string("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
", Stream),
    read_stream_to_lines(Stream, Lines),
    solve(game{red:12, green:13, blue:14}, Lines, Sum, PowerSum).

:- end_tests(day2).
