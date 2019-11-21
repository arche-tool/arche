GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98
GIT_VERSION := $(shell git describe --abbrev=4 --dirty --always --tags)
DIST_DIR := $(shell stack path --dist-dir)
GIT_OK := $(shell ( [ -n '$(git tag --points-at `git rev-parse HEAD`)' ] && [ -z '$(git status -s)' ] ) && echo 1 || echo 0)
SHARED_VOL := /appdata
STACK_ROOT := $(SHARED_VOL)/.stack-root
TARGET_OS := linux
OUTPUT_DIR := .output/$(TARGET_OS)-$(GIT_VERSION)

ifeq ($(OS),Windows_NT)
    OS := Windows
else
    OS := $(shell uname)
endif

ifeq ($(OS),Windows)
	docker build --build-arg USERID=$(shell id -u) -t gamma_stack -f linux.Dockerfile .
    STACK := docker container run -v $(shell pwd):$(SHARED_VOL):Z gamma_stack:latest
endif
ifeq ($(OS),Darwin)
	docker build --build-arg USERID=$(shell id -u) -t gamma_stack -f linux.Dockerfile .
    STACK := docker container run -v $(shell pwd):$(SHARED_VOL):Z gamma_stack:latest
endif
ifeq ($(OS),Linux)
    STACK := stack
endif

.PHONY: all clean

all: stack.yaml.lock cli aws-lambda

clean:
	$(STACK) clean
	docker image prune -f

cli: gamma
	mv $(OUTPUT_DIR)/gamma $(OUTPUT_DIR)/gamma-$(GIT_VERSION)

aws-lambda: gamma-server
	mv $(OUTPUT_DIR)/gamma-server $(OUTPUT_DIR)/bootstrap
	pushd $(OUTPUT_DIR) && zip function-$(GIT_VERSION).zip bootstrap && popd
	rm -f $(OUTPUT_DIR)/bootstrap

stack.yaml.lock: 
	$(STACK) freeze

gamma: install-deps stack.yaml.lock gamma-builder.cabal
	$(STACK) install gamma-builder:exe:gamma --allow-different-user --stack-root $(STACK_ROOT) --local-bin-path $(OUTPUT_DIR)

gamma-server: install-deps stack.yaml.lock gamma-builder.cabal
	$(STACK) install gamma-builder:exe:gamma-server --allow-different-user --stack-root $(STACK_ROOT) --local-bin-path $(OUTPUT_DIR)

install-deps: docker-image
	$(STACK) build --no-terminal --install-ghc --only-dependencies --stack-root $(STACK_ROOT)

docker-image:
	DOCKER_BUILDKIT=1 docker build --build-arg USERID=$(shell id -u) -t gamma_stack -f linux.Dockerfile .