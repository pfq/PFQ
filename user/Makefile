# PFQ Makefile (c) 2011-14 Nicola Bonelli <nicola@pfq.io>
#

MAKE_DIRECTORIES = C C++ test tool Haskell
INST_DIRECTORIES = C C++

.PHONY: all install clean

all: $(MAKE_DIRECTORIES)
clean: $(MAKE_DIRECTORIES)
install: $(INST_DIRECTORIES)

.PHONY: $(MAKE_DIRECTORIES)

$(MAKE_DIRECTORIES):
		@$(MAKE) --keep-going --directory=$@ $(MAKECMDGOALS)

.PHONY: $(MAKECMDGOALS)
		$(MAKECMDGOALS): $(MAKE_DIRECTORIES)

