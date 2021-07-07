guix_configs=$(shell find .config/guix/ -type f -name '*.scm')

systems_targets=$(shell grep -Po '(?<=:tangle\s)([^\s]+)' systems.org | sort -u)
systems_dirs=$(shell echo $(dir $(systems_targets)) | tr ' ' '\n' | sort -u | sed 's;\./;;' | tr -s ' ')

emacs_targets=$(shell grep -Po '(?<=:tangle\s)([^\s]+)' emacs.org | sort -u)
emacs_dirs=$(shell echo $(dir $(emacs_targets)) | tr ' ' '\n' | sort -u | sed 's;\./;;' | tr -s ' ')

all_targets=$($(systems_targets) $(emacs_targets))
all_dirs=$($(systems_targets) $(emacs_targets))

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)       | \
		sed 's/^/  make /'	        | \
		sed 's/:[^#]*[#]# /|/'	        | \
		sed 's/%/LANG/'		        | \
		column -t -s'|' >&2

.PHONY: build
build: tangle ## Ensures that necessary configuration is tangled from Org files

.PHONY: tangle
tangle: $(systems_targets) $(emacs_targets)

.PHONY: install
install: ## Symlinks configuration files into home directory
	stow .

.PHONY: emacs-deps
emacs-deps:
	@echo "Installing Emacs dependencies..."
	@.local/bin/ensure-emacs-deps

.PHONY: clean
clean: ## Removes all configuration files and directories
	rm -f $(all_targets)
	rmdir -p --ignore-fail-on-non-empty $(shell echo $(all_dirs) | sed -e 's;\b[.]/\b;;')

.PHONY: docker
docker: ## Start a Docker container running Emacs
	@.local/bin/emacs-docker

$(systems_dirs):
	mkdir -p $@

$(systems_targets): systems.org $(systems_dirs)
	.bin/tangle-config.el systems.org

$(emacs_dirs):
	mkdir -p $@

$(emacs_targets): emacs.org $(emacs_dirs)
	.bin/tangle-config.el emacs.org
