GHC      ?= ghc
MAIN     ?= Main.hs
BUILD    ?= .build
TARGET   := $(BUILD)/main
GHCFLAGS ?= -Wall -Wcompat -Wincomplete-record-updates -Wredundant-constraints

SOURCES := $(wildcard *.hs)

.PHONY: all build run repl fmt fmt-check tags hoogle clean

all: build

build: $(TARGET)

$(TARGET): $(SOURCES)
	@mkdir -p $(BUILD)
	$(GHC) $(GHCFLAGS) -outputdir $(BUILD) -o $@ $(MAIN)

run: build
	./$(TARGET)

repl:
	ghci $(MAIN)

fmt:
	fourmolu -i $(SOURCES)

fmt-check:
	fourmolu --mode check $(SOURCES)

tags:
	fast-tags $(SOURCES)

hoogle:
	hoogle server --local --port 8080

clean:
	rm -rf $(BUILD) tags
