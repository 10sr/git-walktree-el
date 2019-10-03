emacs ?= emacs

# ert_tests_el = $(wildcard test/*.el)
el := $(wildcard *.el)
elc := $(el:%.el=%.elc)

root := $(CURDIR)

.PHONY: all test test-ert build

all: build

check: build

# test: build test-ert info

build: $(elc)

$(elc): %.elc: %.el
	$(emacs) -batch -Q -L $(root) -L $(root)/tests/lib -f batch-byte-compile $<


clean:
	$(RM) $(elc)
