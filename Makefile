LUA ?= lua

FNLSOURCES = core.fnl core_test.fnl macros_test.fnl
LUASOURCES = $(FNLSOURCES:.fnl=.lua)

all: $(LUASOURCES)

.PHONY: clean help test coverage all

${LUASOURCES}: $(FNLSOURCES)

%.lua: %.fnl
	fennel --lua $(LUA) --compile $< > $@

clean:
	rm -f *.lua luacov*

test:
	@fennel --lua $(LUA) core_test.fnl
	@fennel --lua $(LUA) macros_test.fnl

luacov: | clean all luacov-stats
	luacov

luacov-console: | luacov
	@mv core_test.lua core_test.lua.tmp
	@mv macros_test.lua macros_test.lua.tmp
	luacov-console .
	@mv core_test.lua.tmp core_test.lua
	@mv macros_test.lua.tmp macros_test.lua

luacov-stats: core_test.lua macros_test.lua
	@$(LUA) -lluarocks.loader -lluacov $<

help:
	@echo "make                -- run tests and create lua library" >&2
	@echo "make test           -- run tests" >&2
	@echo "make clean          -- remove lua files" >&2
	@echo "make luacov         -- build coverage report (requires working tests)" >&2
	@echo "make luacov-console -- build coverage report (requires working tests)" >&2
	@echo "make help           -- print this message and exit" >&2
