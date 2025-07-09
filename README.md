# Advent of Code

This repo collects our yearly efforts at completing [Advent of Code][aoc].
The languages for each year are as follows:

* 2017 - Haskell
* 2018 - Haskell
* 2019 - Haskell
* 2020 - Haskell
* 2021 - Haskell
* 2022 - Haskell
* 2023 - Rust
* 2024 - Zig

## 2024  - Zig

This year has been quite interesting using Zig v0.14. Unlike with Haskell
and Rust, there's not such an established package ecosystem or mature
package manager. So we've stuck to the standard library exclusively.

### Usage

Simply `git clone github.com/lawbel/advent-of-code` and `cd 2024` to get a
local copy of the 2024 folder. From there, the usual zig commands should work
as expected. Note again that this project was built with Zig v0.14, so later
versions may require some changes to get working.

* To run each day, do e.g. `zig build -Doptimize=ReleaseFast day-01` for
  day 1. This will compile and then run the solution for day 1 in release
  mode, using the input file `./txt/day_01.txt`. There are other optimisation
  levels, most notably `-Doptimize=Debug` which is the default used if you
  omit the option and just run `zig build day-01`.
* To run all days one after the other,
  do `zig build -Doptimize=ReleaseFast all`.
* To build the whole project run `zig build -Doptimize=ReleaseFast`. This
  will install many executables into `./zig-out/bin` - one for each day and
  one called 'all' that runs every day in order. Note that these executables
  expect input files to be passed over environment variables, so if you want
  to use them directly you'll need to do handle that e.g. by doing
  `ZIG_AOC_DAY_01=$(realpath ./txt/day_01.txt) ./zig-out/bin/day-01`. The
  previously mentioned shortcuts `zig build day-01` take care of this
  environment management automatically for ease of use.
* To test each day, run `zig build test-01 --summary all`. It is also
  possible to point zig at the file for testing directly, by
  doing `zig test ./src/day_01.zig`.
* To test all days at once, run `zig build tests --summary all`, or
  alternatively `zig test ./src/main.zig`.

### Usage - Nix

There is a setup with nix flakes, so it should be possible to do e.g.

```sh
nix run 'github:lawbel/advent-of-code?dir=2024#day-01'
```

to quickly try out day 1 as long as you have [nix][nix] installed. This
command will fetch, compile, and run the code for day 1 using our
input files.

For anything more involved, best to start
with `git clone github.com/lawbel/advent-of-code` and `cd 2024` to get a
local copy of the repo. From there:

* There is a `.envrc` file that will use [direnv][direnv] (or more
  specifically [nix-direnv][nix-direnv]) to evaluate the nix 'devShell'
  and add `zig` and `zls` of appropriate versions to your local environment
  whenever you `cd` into the 2024 folder.
* Alternatively to relying on direnv, a dev shell can be entered directly by
  doing `nix shell`. Again, this will create a local environment with
  executables for Zig and ZLS added into it.
* Running `nix build` will install executables into `./result/bin`, one for
  each day and one called 'all' that runs all days one after the other.
  Like the non-nix usage, these executables expect to get their input file
  communicated over environment variables. So to use them directly do
  something like the following:
  `ZIG_AOC_DAY_01=$(realpath ./txt/day_01.txt) ./result/bin/day-01`.
* Running `nix run .#day-01` will compile and run the code for day 1,
  similarly for other days. This doesn't benefit from caching, so it will
  take a bit to compile and run each time. It *does* however handle the
  environment variables, which can be handy.

[aoc]: https://adventofcode.com/
[nix]: https://nixos.org/
[direnv]: https://direnv.net/
[nix-direnv]: https://github.com/nix-community/nix-direnv
