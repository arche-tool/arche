GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98
GIT_VERSION := "$(shell git describe --abbrev=4 --dirty --always --tags)"
DIST_DIR := "$(shell stack path --dist-dir)"

.PHONY: all clean

all: stack.yaml.lock build

build: $(DIST_DIR)/build/gamma/gamma
	cp $(DIST_DIR)/build/gamma/gamma $(DIST_DIR)/build/gamma/gamma-$(GIT_VERSION)

stack.yaml.lock: 
	stack freeze

clean:
	stack clean

$(DIST_DIR)/build/gamma/gamma: stack.yaml.lock gamma-builder.cabal
	stack build --ghc-options="$(GHCFLAGS)"
