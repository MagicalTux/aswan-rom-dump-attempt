#!/bin/sh
nasm -f bin -o test.ws swandriving.asm
mednafen test.ws
