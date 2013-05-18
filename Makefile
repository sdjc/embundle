# DotEmacs/Makefile
# 2011-8-30

PREFIX=~

empty:

install:
	ln -sfv $(CURDIR)/.emacs $(PREFIX)
	ln -sfv $(CURDIR)/.emacs.d $(PREFIX)
