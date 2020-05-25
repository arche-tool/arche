NPM_BIN_PATH := $(shell npm bin)
ELM_BUILDER := $(NPM_BIN_PATH)/elm-app 

.PHONY: build

build:
	$(ELM_BUILDER) build

run-local:
	$(ELM_BUILDER) start

setup-dev-env:
	npm install --only=dev