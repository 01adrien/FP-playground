TEST_DIR := tests
LIB_SOURCES  := $(wildcard *.rkt)
TEST_SOURCES := $(wildcard $(TEST_DIR)/*-tests.rkt)
ALL_SOURCES  := $(LIB_SOURCES) $(TEST_SOURCES)

RACKET := racket
RACO   := raco

.PHONY: test build clean

test:
	@status=0; \
	for t in $(TEST_SOURCES); do \
	  $(RACKET) $$t || status=1; \
	done; \
	exit $$status

# Un seul module : make test-core lance tests/core-tests.rkt
test-%: $(TEST_DIR)/%-tests.rkt
	@$(RACKET) $<

# Compile sources et tests en bytecode racket
build:
	$(RACO) make -v $(ALL_SOURCES)

clean:
	find . -type d -name compiled -prune -exec rm -rf {} +