guix_configs=$(shell find .config/guix/ -type f -name '*.scm')
systems_targets=$(shell grep -Po '(?<=:tangle\s)([^\s]+)' systems.org | sort -u)
systems_dirs=$(shell echo $(dir $(systems_targets)) | tr ' ' '\n' | sort -u | sed 's;\./;;' | tr -s ' ')

all: $(systems_targets) $(systems_dirs)

$(systems_dirs):
	mkdir -p $@

$(systems_targets): systems.org $(systems_dirs)
	.bin/tangle-config.el systems.org

clean:
	rm -f $(systems_targets)
	rmdir -p --ignore-fail-on-non-empty $(shell echo $(systems_dirs) |  sed -e 's;\b[.]/\b;;')
