SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .el .scm

TARGETS = $(shell grep -Poh '(?<=:tangle\s)([^\s]+)' *.org | sort -u)

dir-exists = $(shell test -d $1 && echo $1)

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

DIRS = $(filter-out ./,$(sort $(foreach f,$(dir $(TARGETS)),$(call dir-exists,$(f)))))

.PHONY: clean
clean: ## Removes all configuration files and directories
	@echo Removing all targets...
	@rm -f $(TARGETS)
	@echo Removing empty target directories...
	@rmdir -p $(DIRS) || \
	echo Target directories are non empty, please delete them and try again

$(TARGETS): systems.org emacs.org
	@.bin/tangle-config.el $^
