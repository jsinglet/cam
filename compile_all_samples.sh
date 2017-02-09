#!/bin/sh

for f in programs/*.cam; do
    echo "Trying to compile $f"
    stack exec camc -- -compile $f
done
