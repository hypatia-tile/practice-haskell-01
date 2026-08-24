GHC       ?= ghc
GHCI      ?= ghci
MAIN      ?= Main.hs
FILE      ?= $(MAIN)
BUILD     ?= .build
TARGET    := $(BUILD)/main

# hie.yaml の cradle arguments と揃えること (docs/adr/0001 参照)
WARNFLAGS ?= -Wall -Wcompat -Wincomplete-record-updates -Wredundant-constraints
GHCFLAGS  ?= $(WARNFLAGS)

SOURCES  := $(wildcard *.hs)
SCRATCH  := $(wildcard Prac_*.hs)
TESTBINS := $(SCRATCH:%.hs=$(BUILD)/test-%)

.PHONY: all build check test run repl fmt fmt-check tags hoogle hooks clean

all: build

build: $(TARGET)

$(TARGET): $(SOURCES)
	@mkdir -p $(BUILD)
	$(GHC) $(GHCFLAGS) -outputdir $(BUILD)/obj-main -o $@ $(MAIN)

# 全ソースを警告込みで型検査する。各ファイルに module ヘッダを付けたので、
# main を持たないファイルも ghc にそのまま渡せる (docs/adr/0002)。
check:
	$(GHC) -fno-code $(WARNFLAGS) -Werror -outputdir $(BUILD)/check $(SOURCES)

# 各 Prac_*.hs が自分の main で自分のテストを走らせる。
# -main-is で Main 以外の名前のモジュールを実行可能にしている。
# ファイルを置けば拾われるので、テスト一覧への登録作業は要らない。
test: $(TESTBINS)
	@for b in $(TESTBINS); do printf '== %s\n' "$$b"; ./"$$b" || exit 1; done

$(BUILD)/test-%: %.hs $(wildcard TestKit.hs)
	@mkdir -p $(BUILD)
	$(GHC) $(GHCFLAGS) -main-is $* -outputdir $(BUILD)/obj-$* -o $@ $<

run: build
	./$(TARGET)

# make repl FILE=Prac_01_ByteString.hs のように対象を指定できる。
repl:
	$(GHCI) $(FILE)

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
