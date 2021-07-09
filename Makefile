SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .el .scm

TARGETS = $(shell grep -Poh '(?<=:tangle\s)([^\s]+)' *.org | sort -u)

.PHONY: help
help: ## Display this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)        | \
		sed 's/^/  make /'	         | \
		sed 's/:[^#]*[#]# /|/'	         | \
		sed 's/%/LANG/'		         | \
		column -t -s'|' >&2

.PHONY: build
build: $(TARGETS)

.PHONY: install
install: ## Symlinks configuration files into home directory
	@echo Symlinking configuration files into ${HOME}
	@stow .

.PHONY: clean
clean: ## Removes all configuration files and directories
	@echo Removing all targets...
	@rm -f $(TARGETS)
	@echo Removing empty target directories...
	@rmdir -p --ignore-fail-on-non-empty $(dir $(TARGETS))

$(TARGETS): systems.org emacs.org
	@.bin/tangle-config.el $^
