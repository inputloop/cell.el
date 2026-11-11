# Makefile --- build and test recipe for cell-mode  -*- makefile-gmake -*-
#
# Usage:
#   make            byte-compile, then run the ERT suite
#   make compile    byte-compile cell-mode.el (warnings are shown, not fatal)
#   make test       run the ERT suite in t/ against the source
#   make prove-red  show the suite can fail: a planted failure must turn it red
#   make clean      remove compiled files
#
# EMACS may be overridden, e.g.  make EMACS=/opt/emacs-30/bin/emacs test

EMACS ?= emacs
BATCH  = $(EMACS) -Q --batch -L .

SRC   = cell-mode.el
TESTS = t/cell-mode-tests.el

.PHONY: all compile test prove-red clean

all: compile test

compile: $(SRC:.el=.elc)

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<

test:
	$(BATCH) -l $(SRC) -l $(TESTS) -f ert-run-tests-batch-and-exit

# A green suite that cannot fail is indistinguishable from one that is
# not running.  CELL_MODE_PROVE_RED=1 enables one test that must fail;
# this target succeeds only when the suite then exits non-zero.
prove-red:
	@if CELL_MODE_PROVE_RED=1 $(MAKE) --no-print-directory test >/dev/null 2>&1; \
	then echo "prove-red: FAULT - the planted failure did not turn the suite red"; exit 1; \
	else echo "prove-red: ok - the suite goes red when a test fails"; fi

clean:
	rm -f *.elc t/*.elc

# Copyright (C) 2026 Corwin Brust, Bryant Frankford (INPUTLOOP)
# SPDX-License-Identifier: GPL-3.0-or-later
