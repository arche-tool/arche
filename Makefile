GIT_OK := $(shell ( [ -n '$(git tag --points-at `git rev-parse HEAD`)' ] && [ -z '$(git status -s)' ] ) && echo 1 || echo 0)
GIT_VERSION := $(shell git describe --abbrev=4 --dirty --always --tags)
SHARED_VOL := /appdata
OUTPUT_ROOT_DIR := .output

ifeq ($(OS),Windows_NT)
  HOST_OS := windows
else
  HOST_OS := $(shell uname | tr '[:upper:]' '[:lower:]')
endif

BUILD_NAME := $(HOST_OS)-$(GIT_VERSION)
OUTPUT_DIR := $(OUTPUT_ROOT_DIR)/$(BUILD_NAME)

GC_PROJ := apt-muse-269419
GCR_HOST := gcr.io
ARCHE_DOCKER_NAME := $(GCR_HOST)/$(GC_PROJ)/arche_server-$(BUILD_NAME)

STACK_ARGS := --local-bin-path $(OUTPUT_DIR)
STACK := stack
BUILD_STACK_IMAGE :=

ifdef CI
  ifeq ($(CI),true)
	STACK_ARGS := --system-ghc --local-bin-path $(OUTPUT_DIR)
	STACK := stack
	BUILD_STACK_IMAGE :=
  endif
endif

ifdef BUILD_ON_DOCKER
  ifeq ($(CI),true)
	STACK_ROOT := $(SHARED_VOL)/.stack-root
	STACK_ARGS := --allow-different-user --stack-root $(STACK_ROOT) --local-bin-path $(OUTPUT_DIR)
	STACK := docker container run -v $(shell pwd):$(SHARED_VOL):Z arche_stack:latest
	BUILD_STACK_IMAGE := arche_stack_image
  endif
endif

.PHONY: all clean

all: arche-cli

clean-stack:
	$(STACK) clean

clean:
	rm -rf $(OUTPUT_ROOT_DIR)/*
	docker rmi $(docker images 'arche_server-*' -q) --force
	docker image prune -f

stack.yaml.lock: 
	$(STACK) freeze

arche-cli: build stack.yaml.lock arche.cabal
	$(STACK) install arche:exe:arche --flag arche:cli $(STACK_ARGS)

arche-server: build stack.yaml.lock arche.cabal
	$(STACK) install arche:exe:arche-server --flag arche:server $(STACK_ARGS)

build: $(BUILD_STACK_IMAGE) 
	$(STACK) build --no-terminal --test --bench --no-run-tests --no-run-benchmarks $(STACK_ARGS)

arche_stack_image:
	docker build --build-arg USERID=$(shell id -u) -t arche_stack -f linux.Dockerfile .

run-server: docker_server_image-$(BUILD_NAME)
	docker container run -p 8888:8080 arche_server-$(BUILD_NAME):latest

run-bench: build stack.yaml.lock arche.cabal
	$(STACK) bench arche:arche-bench $(STACK_ARGS)

run-test: build stack.yaml.lock arche.cabal
	$(STACK) test arche $(STACK_ARGS)

ifdef GCLOUD_SERVICE_KEY

docker_server_image-$(BUILD_NAME): arche-serverdocker build \
	--build-arg BUILD_NAME=$(BUILD_NAME) \
	--build-arg GCLOUD_SERVICE_KEY \
	-t $(ARCHE_DOCKER_NAME) \
	-t arche_server-$(BUILD_NAME) \
	-f server.Dockerfile .

deploy-server: docker_server_image-$(BUILD_NAME)
	@echo $$(echo "$$GCLOUD_SERVICE_KEY" | base64 -d | docker login -u _json_key --password-stdin https://$(GCR_HOST)/)
	docker push $(ARCHE_DOCKER_NAME)
endif

rename-binaries:
	@ls -lah ".output/$(HOST_OS)-$(GIT_VERSION)/"
	@for f in $$(find .output/$(HOST_OS)-$(GIT_VERSION)/* -maxdepth 0); do \
		filename=$$(basename "$$f"); \
		ext=$$([[ "$$filename" = *.* ]] && echo ".$${filename##*.}" || echo ''); \
		mv -v "$$f" "$$(dirname "$$f")/$(HOST_OS)-$$(basename "$$f" $$ext)-$(GIT_VERSION)$$ext"; \
	done

show-output-dir:
	@echo $(OUTPUT_DIR)