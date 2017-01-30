#!/bin/sh

for f in programs/*.cam; do
    stack exec cam -- -compile $f
done
