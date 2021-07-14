SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .el .scm

# See: https://stackoverflow.com/a/14061796
# If the first argument is one of the following, treat the remaining
# arguments as arguments to a subcommand.
ifeq (emacs,$(firstword $(MAKECMDGOALS)))
    RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
    # Turn the remaining arguments into do-nothing targets.
    $(eval $(RUN_ARGS):;@:)
endif

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

subcmd: #
	@#

.PHONY: emacs
emacs : subcmd build
	@$(MAKE) -C .emacs.d $(RUN_ARGS)

$(TARGETS): systems.org emacs.org
	@scripts/tangle-config.el $^
