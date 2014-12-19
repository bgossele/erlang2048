erlang2048
==========

An Erlang-implementation of the popular game 2048. It focusses on concurrency and fault-tolerance, and was a project for the course [Comparative Programming Languages](https://onderwijsaanbod.kuleuven.be/syllabi/e/H0S01AE.htm) at KU Leuven.

To compile everything in the /src folder, execute `erlc *.erl` (Unix) or `erl -make` (Windows).

To start playing, run `main:play().`, or `main:playnoblaster().` to play without the blaster randomly killing tiles.
Play with the WASD- or IJKL-keys, or enter `h` for a list of available commands.

To switch debug statements on or off, change line 16 in debug.erl.
