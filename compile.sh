#!/bin/bash
make
rm a.out
./pascaml $1
gcc -no-pie -g test.s -o a.out
./a.out
