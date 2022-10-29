LUA ?= lua
FENNEL ?= fennel
VERSION ?= $(shell git describe --abbrev=0)
FNLSOURCES = init.fnl
FNLMACROS = init-macros.fnl
FNLTESTS = $(wildcard tests/*.fnl) fennel-test/utils.fnl
LUATESTS = $(FNLTESTS:.fnl=.lua)
FNLDOCS = $(FNLMACROS) $(FNLSOURCES)
LUASOURCES = $(FNLSOURCES:.fnl=.lua)
LUAEXECUTABLES ?= lua luajit
FENNELDOC := $(shell command -v fenneldoc)
LUACOV_COBERTURA := $(shell command -v luacov-cobertura)
COMPILEFLAGS += --metadata --require-as-include

.PHONY: build clean distclean test luacov luacov-console doc help $(LUAEXECUTABLES)

build: $(LUASOURCES)
	@echo "--[[ This is a self-contained version of the fennel-cljlib library" > cljlib.lua
	@echo "     meant to be used directly from Lua, or embedded into other" >> cljlib.lua
	@echo "     applications. It doesn't include macros, given that Lua doesn't" >> cljlib.lua
	@echo "     support Fennel's macro system, but all other features, like" >> cljlib.lua
	@echo "     laziness, and immutability are available in the same way as if" >> cljlib.lua
	@echo "     this library was used from Fennel. ]]" >> cljlib.lua
	@cat init.lua >> cljlib.lua

${LUASOURCES}: $(FNLSOURCES)

%.lua: %.fnl
	$(FENNEL) --lua $(LUA) $(COMPILEFLAGS) --compile $< > $@

clean:
	rm -f $(LUASOURCES) $(LUATESTS) cljlib.lua

distclean: clean
	rm -rf luacov* coverage

test: COMPILEFLAGS = --metadata
test: $(FNLTESTS)
	@echo "Testing on" $$($(LUA) -v) >&2
	@$(foreach test,$?,LUA_PATH="./?/init.lua;$LUA_PATH" $(FENNEL) $(COMPILEFLAGS) --lua $(LUA) $(test) || exit;)
ifdef FENNELDOC
	@fenneldoc --mode check $(FNLDOCS) || exit
else
	@echo "" >&2
	@echo "fenneldoc is not installed" >&2
	@echo "Please install fenneldoc to check documentation during testing" >&2
	@echo "https://gitlab.com/andreyorst/fenneldoc" >&2
	@echo "" >&2
endif

testall: $(LUAEXECUTABLES)
	@$(foreach lua,$?,LUA=$(lua) make test || exit;)

luacov: COMPILEFLAGS = --correlate --metadata
luacov: distclean build $(LUATESTS)
	@$(foreach test,$(LUATESTS),$(LUA) -lluarocks.loader -lluacov $(test) || exit;)
	luacov
ifdef LUACOV_COBERTURA
	mkdir -p coverage
	luacov-cobertura -o coverage/cobertura-coverage.xml
endif

luacov-console: COMPILEFLAGS = --correlate --metadata
luacov-console: clean build $(LUATESTS)
	@$(foreach test,$(LUATESTS),$(LUA) -lluarocks.loader -lluacov $(test) || exit;)
	luacov
	luacov-console .
	luacov-console --no-colored -s

doc:
ifdef FENNELDOC
	fenneldoc --project-version $(VERSION) --config $(FNLMACROS) $(FNLSOURCES)
else
	@echo "" >&2
	@echo "fenneldoc is not installed" >&2
	@echo "Visit https://gitlab.com/andreyorst/fenneldoc for installation instructions" >&2
	@echo "" >&2
endif

help:
	@echo "make                -- create lua library" >&2
	@echo "make clean          -- remove lua files" >&2
	@echo "make distclean      -- remove all files not necessary for the project" >&2
	@echo "make luacov         -- run tests to produce luacov report" >&2
	@echo "make luacov-console -- run tests to produce luacov-console report" >&2
	@echo "make doc            -- create documentation with fenneldoc" >&2
	@echo "make help           -- print this message and exit" >&2
