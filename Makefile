# Configuration for Make
MAKEFLAGS += --warn-undefined-variables
.PHONY: install clean build test run repl help
#.SILENT:

# Executables used in this makefile
YARN := yarn
NPX := npx
PULP := pulp
BOWER := bower
ELECTRON := electron
MINIFY := minify
ELECTRON-PACKAGER := electron-packager

# Options to pass to pulp when building
BUILD_OPTIONS := -- --stash --censor-lib --strict

# Package manifest files
PACKAGEJSON := package.json
BOWERJSON := bower.json

# Various input directories
SRCPATH := ./src
TESTPATH := ./test

# Various output directories
COMPONENTS := ./bower_components
MODULES := ./node_modules
OUTPUT := ./output
DIST := ./dist
prefix :=

# Globs that match all source files
SOURCES := $(SRCPATH)/**/*
TESTSOURCES := $(TESTPATH)/**/*

#
$(MODULES): $(PACKAGEJSON)
	$(YARN) --cache-folder $(MODULES) install

#
$(COMPONENTS): $(BOWERJSON) $(MODULES)
	$(BOWER) install

# Build the source files
$(OUTPUT): $(SOURCES) $(COMPONENTS) $(MODULES)
	$(PULP) build \
	  --src-path $(SRCPATH) \
	  --build-path $(OUTPUT) \
	  $(BUILD_OPTIONS)
	touch $(OUTPUT)
build: $(OUTPUT)

# Build the minified, concatenatted bundles that are distributed outside
# development
$(DIST):
	mkdir $(DIST)
$(DIST)/package.json: $(DIST) $(PACKAGEJSON)
	cp $(PACKAGEJSON) $(DIST)/package.json
$(OUTPUT)/index.js: $(OUTPUT) $(COMPONENTS) $(MODULES)
	$(PULP) build \
	  --main Minichrome.CLI \
	  --to $(OUTPUT)/index.js
$(DIST)/index.js: $(DIST) $(OUTPUT)/index.js $(MODULES)
	$(MINIFY) $(OUTPUT)/index.js -o $(DIST)/index.js
$(OUTPUT)/ui.js: $(OUTPUT) $(COMPONENTS) $(MODULES)
	$(PULP) build \
	  --main Minichrome.UI \
	  --to $(OUTPUT)/ui.js
$(DIST)/ui.js: $(DIST) $(OUTPUT)/ui.js $(MODULES)
	$(MINIFY) $(OUTPUT)/ui.js -o $(DIST)/ui.js
$(OUTPUT)/page.js: $(OUTPUT) $(COMPONENTS) $(MODULES)
	$(PULP) build \
	  --main Minichrome.Page \
	  --to $(OUTPUT)/page.js
$(DIST)/page.js: $(DIST) $(OUTPUT)/page.js $(MODULES)
	$(MINIFY) $(OUTPUT)/page.js -o $(DIST)/page.js

# Package the distribution files
$(DIST)/minichrome-linux-x64: $(DIST)/page.js $(DIST)/ui.js $(DIST)/index.js $(DIST)/package.json $(MODULES)
	cd $(DIST)
	$(ELECTRON-PACKAGER) ./ --asar --out $(DIST)

install: $(DIST)/minichrome-linux-x64
	for file in $$(find $(DIST)/minichrome-linux-x64 -type f); do \
	    install -m644 -D $$file $(prefix)/lib/minichrome/$${file#$(DIST)/minichrome-linux-x64/}; \
	done
	rm $(prefix)/lib/minichrome/minichrome
	install -m755 -D $(DIST)/minichrome-linux-x64/minichrome $(prefix)/bin/minichrome

# Run the test suite
test: $(OUTPUT) $(TESTSOURCES) $(COMPONENTS) $(MODULES)
	$(PULP) test \
	  --src-path $(SRCPATH) \
	  --test-path $(TESTPATH) \
	  --build-path $(OUTPUT) \
	  $(BUILD_OPTIONS)

# Build and run the app
run: $(OUTPUT)
	$(ELECTRON) .

# Launch a repl with all modules loaded
repl: $(SOURCES) $(TESTSOURCES) $(COMPONENTS) $(MODULES)
	$(PULP) repl \
	  --src-path $(SRCPATH) \
	  --test-path $(TESTPATH)

# Remove all make output from the source tree
clean:
	rm -rf $(OUTPUT) $(DIST) $(COMPONENTS) $(MODULES)

# Print out a description of all the supported tasks
help:
	$(info HTTPure make utility)
	$(info )
	$(info Usage: make [ install | build | dist | run | test | repl | clean | help ])
	$(info )
	$(info - make             Build the distribution)
	$(info - make install     Install the distribution, optionally to $$prefix)
	$(info - make build       Build the source code)
	$(info - make run         Run the development build of the application)
	$(info - make test        Run the test suite)
	$(info - make repl        Launch a repl with all project code loaded)
	$(info - make clean       Remove all build files)
	$(info - make help        Print this help)

.DEFAULT_GOAL := $(DIST)/minichrome-linux-x64
