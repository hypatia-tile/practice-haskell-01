GHC       ?= ghc
GHCI      ?= ghci
MAIN      ?= Main.hs
BUILD     ?= .build
TARGET    := $(BUILD)/main

# hie.yaml の cradle arguments と揃えること (docs/adr/0001 参照)
WARNFLAGS ?= -Wall -Wcompat -Wincomplete-record-updates -Wredundant-constraints
GHCFLAGS  ?= $(WARNFLAGS)

SOURCES := $(wildcard *.hs)

.PHONY: all build check run repl fmt fmt-check tags hoogle hooks clean

all: build

build: $(TARGET)

$(TARGET): $(SOURCES)
	@mkdir -p $(BUILD)
	$(GHC) $(GHCFLAGS) -outputdir $(BUILD) -o $@ $(MAIN)

# 全ソースを警告込みで型検査する。ghc ではなく ghci を使うのは、
# main を持たないスクラッチファイルを ghc に渡すと
# 「The IO action 'main' is not defined」で落ちるため (issue #5)。
# ghci は main を要求しないので、-Werror と組み合わせれば警告の門番になる。
check:
	@for f in $(SOURCES); do \
		printf '  typecheck %s\n' "$$f"; \
		$(GHCI) $(WARNFLAGS) -Werror -v0 -e 'return ()' "$$f" || exit 1; \
	done

run: build
	./$(TARGET)

repl:
	$(GHCI) $(MAIN)

fmt:
	fourmolu -i $(SOURCES)

fmt-check:
	fourmolu --mode check $(SOURCES)

tags:
	fast-tags $(SOURCES)

hoogle:
	hoogle server --local --port 8080

# .githooks/ を git に使わせる。clone 直後に一度だけ実行する。
hooks:
	git config core.hooksPath .githooks
	@echo "core.hooksPath = .githooks を設定しました"

clean:
	rm -rf $(BUILD) tags
