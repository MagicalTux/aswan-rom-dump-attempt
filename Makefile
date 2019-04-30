#!/bin/make

all: test.ws

test.ws: test.asm
	nasm -f bin -o $@ $<

test: test.ws
	mednafen $<

dump: test.ws
	objdump -D -Mintel,i8086 -b binary -m i386 $< | grep -v '(bad)'
