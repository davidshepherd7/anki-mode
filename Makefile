EMACS=emacs
CASK ?= cask

build :
	cask exec $(EMACS) -Q --batch --eval             \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" anki-mode.el

clean :
	@rm -f *.elc

test: build
	cask exec ert-runner

install:
	${CASK} install

.PHONY:	all test install clean build
