GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98
GIT_VERSION := $(shell git describe --abbrev=4 --dirty --always --tags)
DIST_DIR := $(shell stack path --dist-dir)
GIT_OK := $(shell ( [ -n '$(git tag --points-at `git rev-parse HEAD`)' ] && [ -z '$(git status -s)' ] ) && echo 1 || echo 0)

.PHONY: all clean

all: stack.yaml.lock build

build: $(DIST_DIR)/build/arche/arche
	cp $(DIST_DIR)/build/arche/arche $(DIST_DIR)/build/arche/arche$(GIT_VERSION)

stack.yaml.lock: 
	stack freeze

clean:
	stack clean

$(DIST_DIR)/build/arche/arche: stack.yaml.lock arche.cabal
	stack build --ghc-options="$(GHCFLAGS)"

$(info $(GIT_OK))
