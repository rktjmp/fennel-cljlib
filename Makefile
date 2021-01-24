LUA ?= lua
FENNEL ?= fennel
FNLSOURCES = init.fnl
LUASOURCES = $(FNLSOURCES:.fnl=.lua)
FNLTESTS = tests/fn.fnl tests/macros.fnl tests/core.fnl
FNLDOCS = init.fnl macros.fnl tests/test.fnl
LUATESTS = $(FNLTESTS:.fnl=.lua)
LUA_EXECUTABLES ?= lua luajit
FENNELDOC := $(shell command -v fenneldoc)

.PHONY: build clean distclean help test luacov luacov-console fenneldoc $(LUA_EXECUTABLES)

build: $(LUASOURCES)

${LUASOURCES}: $(FNLSOURCES)

%.lua: %.fnl
	$(FENNEL) --lua $(LUA) --compile $< > $@

clean:
	rm -f $(LUASOURCES) $(LUATESTS)

distclean: clean
	rm -f luacov*

test: $(FNLTESTS)
	@echo "Testing on" $$($(LUA) -v) >&2
ifdef FENNELDOC
	@fenneldoc --mode check $(FNLDOCS) || exit
else
	@echo ""
	@echo "fenneldoc is not installed" >&2
	@echo "Please install fenneldoc to check documentation during testing" >&2
	@echo "https://gitlab.com/andreyorst/fenneldoc" >&2
	@echo ""
endif
	@$(foreach test,$?,$(FENNEL) --lua $(LUA) --metadata $(test) || exit;)

testall: $(LUA_EXECUTABLES)
	@$(foreach lua,$?,LUA=$(lua) make test || exit;)

luacov: build $(LUATESTS)
	@$(foreach test,$(LUATESTS),$(LUA) -lluarocks.loader -lluacov $(test) || exit;)
	luacov

luacov-console: luacov
	@$(foreach test, $(LUATESTS), mv $(test) $(test).tmp;)
	luacov-console .
	@$(foreach test, $(LUATESTS), mv $(test).tmp $(test);)

fenneldoc:
	fenneldoc $(FNLDOCS)

help:
	@echo "make                -- run tests and create lua library" >&2
	@echo "make test           -- run tests" >&2
	@echo "make clean          -- remove lua files" >&2
	@echo "make distclean      -- remove all unnecessary files" >&2
	@echo "make luacov         -- build coverage report" >&2
	@echo "make luacov-console -- build coverage report for luacov-console" >&2
	@echo "make help           -- print this message and exit" >&2

-include .depend.mk
