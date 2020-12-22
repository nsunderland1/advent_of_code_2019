#!/bin/bash

DAY=$1
DAY_PADDED=$(printf "%02d" $DAY)

firefox https://adventofcode.com/2019/day/${DAY}

mkdir day${DAY_PADDED}
cp template-dune day${DAY_PADDED}/dune
cp template.ml day${DAY_PADDED}/puzzle.ml

# generate .merlin files to make ocaml-lsp happy
dune build day${DAY_PADDED}

code . day${DAY_PADDED}/puzzle.ml

curl "https://adventofcode.com/2019/day/${DAY}/input" -H "Cookie: session=${ADVENT_SESSION}" > "day${DAY_PADDED}/input"
