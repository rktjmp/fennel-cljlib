FNLSOURCES = core.fnl
LUASOURCES = $(FNLSOURCES:.fnl=.lua)

all: cljlib.lua

.PHONY: all clean help test

cljlib.lua: test $(LUASOURCES)
	mv core.lua cljlib.lua

%.lua: %.fnl
	fennel --compile $^ > $@

clean:
	rm -f *.lua

test:
	@fennel core_test.fnl
	@fennel macros_test.fnl

help:
	@echo "make       -- run tests and create lua library"
	@echo "make test  -- run tests"
	@echo "make clean -- remove lua files"
