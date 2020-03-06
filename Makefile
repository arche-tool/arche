GIT_VERSION := $(shell git describe --abbrev=4 --dirty --always --tags)
GIT_OK := $(shell ( [ -n '$(git tag --points-at `git rev-parse HEAD`)' ] && [ -z '$(git status -s)' ] ) && echo 1 || echo 0)
SHARED_VOL := /appdata
STACK_ROOT := $(SHARED_VOL)/.stack-root
TARGET_OS := linux
OUTPUT_ROOT_DIR := .output
BUILD_NAME := $(TARGET_OS)-$(GIT_VERSION)
OUTPUT_DIR := $(OUTPUT_ROOT_DIR)/$(BUILD_NAME)
STACK_ARGS := --allow-different-user --stack-root $(STACK_ROOT) --local-bin-path $(OUTPUT_DIR)

GC_PROJ := apt-muse-269419

ifeq ($(OS),Windows_NT)
    OS := Windows
else
    OS := $(shell uname)
endif

ifeq ($(OS),Windows)
	docker build --build-arg USERID=$(shell id -u) -t arche_stack -f linux.Dockerfile .
    STACK := docker container run -v $(shell pwd):$(SHARED_VOL):Z arche_stack:latest
endif
ifeq ($(OS),Darwin)
	docker build --build-arg USERID=$(shell id -u) -t arche_stack -f linux.Dockerfile .
    STACK := docker container run -v $(shell pwd):$(SHARED_VOL):Z arche_stack:latest
endif
ifeq ($(OS),Linux)
    STACK := stack
endif

.PHONY: all clean

all: stack.yaml.lock cli aws-lambda

clean-stack:
	$(STACK) clean

clean:
	rm -rf $(OUTPUT_ROOT_DIR)/*
	docker rmi $(docker images 'arche_server-*' -q) --force
	docker image prune -f

cli: arche
	mv $(OUTPUT_DIR)/arche $(OUTPUT_DIR)/arche-$(GIT_VERSION)

aws-lambda: arche-server-$(BUILD_NAME)
	cp $(OUTPUT_DIR)/arche-server $(OUTPUT_DIR)/bootstrap
	pushd $(OUTPUT_DIR) && zip function-$(GIT_VERSION).zip bootstrap && popd
	rm -f $(OUTPUT_DIR)/bootstrap

stack.yaml.lock: 
	$(STACK) freeze

arche: install-deps stack.yaml.lock arche.cabal
	$(STACK) install arche:exe:arche --flag arche:cli $(STACK_ARGS)

arche-server-$(BUILD_NAME): install-deps stack.yaml.lock arche.cabal
	$(STACK) install arche:exe:arche-server --flag arche:server $(STACK_ARGS)

install-deps: docker-image
	$(STACK) build --no-terminal --install-ghc --only-dependencies --stack-root $(STACK_ROOT)

docker-image:
	DOCKER_BUILDKIT=1 docker build --build-arg USERID=$(shell id -u) -t arche_stack -f linux.Dockerfile .

docker_server_image-$(BUILD_NAME): arche-server-$(BUILD_NAME)
	DOCKER_BUILDKIT=1 docker build --build-arg BUILD_NAME=$(BUILD_NAME) -t gcr.io/$(GC_PROJ)/arche_server-$(BUILD_NAME) -t arche_server-$(BUILD_NAME) -f server.Dockerfile .

run-server: docker_server_image-$(BUILD_NAME)
	docker container run -p 8888:8080 arche_server-$(BUILD_NAME):latest

run-bench: install-deps stack.yaml.lock arche.cabal
	$(STACK) bench arche:arche-bench $(STACK_ARGS)

deploy-server: docker_server_image-$(BUILD_NAME)
	docker push gcr.io/$(GC_PROJ)/arche_server-$(BUILD_NAME)
