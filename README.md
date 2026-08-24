# Haskell practice

Haskell の練習用リポジトリ。Cabal project は作らず、Nix の `ghcWithPackages` で
用意した GHC を直接叩く構成にしている。理由と代償は
[docs/adr/0001-ghc-with-packages-without-cabal.md](docs/adr/0001-ghc-with-packages-without-cabal.md) を参照。

## セットアップ

direnv を使う場合は `direnv allow` で dev shell に入る。使わない場合は `nix develop`。

clone 直後に `make hooks` を一度実行して pre-commit hook を有効化する
(git は `.githooks/` を自動では見ないため、`core.hooksPath` の設定が要る)。

## 使い方

| コマンド | 内容 |
| --- | --- |
| `make` / `make build` | `Main.hs` をビルドする (成果物は `.build/`) |
| `make run` | ビルドして実行する |
| `make test` | 各 `Prac_*.hs` のテストをビルドして実行する |
| `make check` | 全ソースを `-Wall -Werror` で型検査する |
| `make repl` | `ghci` を起動する (`make repl FILE=Prac_01_ByteString.hs`) |
| `make fmt` | fourmolu で整形する |
| `make fmt-check` | 整形済みか検査する |
| `make hooks` | pre-commit hook を有効化する |
| `make tags` | fast-tags で `tags` を生成する |
| `make hoogle` | ローカル Hoogle を http://localhost:8080 で起動する |
| `make clean` | `.build/` と `tags` を削除する |

## 練習ファイルを追加する

`Prac_NN_Topic.hs` を作り、`module Prac_NN_Topic where` を付けて `main` を書く。
`main` がそのファイルのテストを走らせる。

```haskell
module Prac_02_Something where

import TestKit (runCases, runTests)

main :: IO ()
main = runTests [runCases "f" f [(input, expected), ...]]
```

`make test` は `$(wildcard Prac_*.hs)` を拾うので、**どこにも登録しなくてよい**。
`ghc -main-is Prac_NN_Topic` でビルドされるため、モジュール名が `Main` でなくても
実行ファイルになる。詳細は
[docs/adr/0002-practice-files-carry-their-own-tests.md](docs/adr/0002-practice-files-carry-their-own-tests.md)。

期待値を書き下すケースに加えて、**不変条件**も書いておくと入力を足したときにタダで効く
(`Prac_01` では「Span で切り出して繋ぎ直すと元に戻る」を検査している)。

## pre-commit hook

private repository なので CI は置かず、commit 時に `make fmt-check` / `make check` /
`make test` を回す。作業ツリーではなく**ステージされた内容**を検査するので、
編集途中のファイルがあってもコミットは通る。

意図的に落としたいときは `git commit --no-verify`。

## パッケージを追加する

`flake.nix` の `ghcWithPackages (p: [ ... ])` にパッケージ名を足して `direnv reload`
(direnv を使わないなら `nix develop` に入り直す)。`build-depends` に相当する宣言は
どこにも要らず、そのまま import できる。

ただし宣言していないパッケージも推移閉包経由で import できてしまうので、
**使うものは必ず `flake.nix` に書くこと**。
