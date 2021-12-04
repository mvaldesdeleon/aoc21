# Advent of Code 2021

Advent of Code 2021, in Haskell

http://adventofcode.com/2021

# Instructions

Before you compile the code, you'll need to create the `Session` module at **src/Session.hs** and provide your session cookie. This will download your input files automatically, and cache them.

```haskell
module Session where

import Relude.String (ByteString)

session :: ByteString
session = "<YOUR SESSION COOKIE HERE>"
```

You should have [stack](https://docs.haskellstack.org/en/stable/README/) installed before continuing.

```sh
git clone git@github.com:mvaldesdeleon/aoc21.git
cd aoc21
stack build
stack exec aoc21 day[1-4]
# i.e., stack exec aoc21 day1
```

# Live Stream

I'll be live streaming my solutions on [twitch](https://www.twitch.tv/mvaldesdeleon). No fixed schedule, but I would expect something between 18:00 and 22:00 CET.

# License

BSD-3-Clause