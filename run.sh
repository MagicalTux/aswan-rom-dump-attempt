#!/bin/sh
set -e
nasm -f bin -o test.ws swandriving.asm
mednafen test.ws
