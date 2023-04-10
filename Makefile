SHELL = /bin/bash
PKG = trace-tree

all:
	@

README.md: el2markdown.el ${PKG}.el
	emacs -batch -l $< ${PKG}.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	wget \
  -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"
