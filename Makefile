LUA ?= lua

FNLSOURCES = core.fnl test/core.fnl test/macros.fnl
LUASOURCES = $(FNLSOURCES:.fnl=.lua)

all: $(LUASOURCES)

.PHONY: clean help test coverage all

${LUASOURCES}: $(FNLSOURCES)

%.lua: %.fnl
	fennel --lua $(LUA) --compile $< > $@

clean:
	rm -f *.lua
	rm -f test/*.lua

clean-all: clean
	rm -f luacov*

test: clean
	@fennel --lua $(LUA) test/fn.fnl
	@fennel --lua $(LUA) test/core.fnl
	@fennel --lua $(LUA) test/macros.fnl

luacov: | clean-all all luacov-stats
	luacov

luacov-console: | luacov
	@mv test/core.lua test/core.lua.tmp
	@mv test/macros.lua test/macros.lua.tmp
	@mv test/fn.lua test/fn.lua.tmp
	luacov-console .
	@mv test/core.lua.tmp test/core.lua
	@mv test/macros.lua.tmp test/macros.lua
	@mv test/fn.lua.tmp test/fn.lua

luacov-stats: test/core.lua test/macros.lua test/fn.lua
	@$(LUA) -lluarocks.loader -lluacov $<

help:
	@echo "make                -- run tests and create lua library" >&2
	@echo "make test           -- run tests" >&2
	@echo "make clean          -- remove lua files" >&2
	@echo "make luacov         -- build coverage report (requires working tests)" >&2
	@echo "make luacov-console -- build coverage report (requires working tests)" >&2
	@echo "make help           -- print this message and exit" >&2

-include .depend.mk
