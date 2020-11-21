LUA ?= lua
FENNEL ?= fennel
FNLSOURCES = cljlib.fnl
LUASOURCES = $(FNLSOURCES:.fnl=.lua)
FNLTESTS = tests/fn.fnl tests/macros.fnl tests/core.fnl
LUATESTS = $(FNLTESTS:.fnl=.lua)

.PHONY: build clean distclean help test luacov luacov-console fenneldoc

build: $(LUASOURCES)

${LUASOURCES}: $(FNLSOURCES)

%.lua: %.fnl
	$(FENNEL) --lua $(LUA) --compile $< > $@

clean:
	rm -f $(LUASOURCES) $(LUATESTS)

distclean: clean
	rm -f luacov*

test: $(FNLTESTS)
	@true$(foreach test, $?, && $(FENNEL) --lua $(LUA) --metadata $(test))

luacov: build $(LUATESTS)
	@true$(foreach test, $(LUATESTS), && $(LUA) -lluarocks.loader -lluacov $(test))
	luacov

luacov-console: luacov
	@$(foreach test, $(LUATESTS), mv $(test) $(test).tmp;)
	luacov-console .
	@$(foreach test, $(LUATESTS), mv $(test).tmp $(test);)

fenneldoc:
	fenneldoc cljlib.fnl cljlib-macros.fnl tests/test.fnl

help:
	@echo "make                -- run tests and create lua library" >&2
	@echo "make test           -- run tests" >&2
	@echo "make clean          -- remove lua files" >&2
	@echo "make distclean      -- remove all unnecessary files" >&2
	@echo "make luacov         -- build coverage report" >&2
	@echo "make luacov-console -- build coverage report for luacov-console" >&2
	@echo "make help           -- print this message and exit" >&2

-include .depend.mk
