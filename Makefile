FNLSOURCES = core.fnl core_test.fnl macros_test.fnl
LUASOURCES = $(FNLSOURCES:.fnl=.lua)

all: $(LUASOURCES)

.PHONY: clean help test coverage all

${LUASOURCES}: $(FNLSOURCES)

%.lua: %.fnl
	fennel --compile $< > $@

clean:
	rm -f *.lua luacov*

test:
	@fennel core_test.fnl
	@fennel macros_test.fnl

coverage: | clean all luacov-stats
	luacov

luacov-stats: core_test.lua macros_test.lua
	@lua -lluarocks.loader -lluacov $<

help:
	@echo "make       -- run tests and create lua library"
	@echo "make test  -- run tests"
	@echo "make clean -- remove lua files"
