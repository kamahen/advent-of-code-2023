# Advent of Code 2023

See also https://adventofcode.com/2023

All of these were done with [SWI-Prolog](https://www.swi-prolog.org/) unless otherwise noted.

## Day 1

Quite straightforward.

The modifications for the 2nd part were simple:
* added a `fwd` or `rev` parameter to `first_digit//2`.
* changed `first_digit//2` to call `digit//2`, with additional cases for `"zero"`, `"one"`, etc.
