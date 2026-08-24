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
| `make check` | 全ソースを `-Wall -Werror` で型検査する |
| `make repl` | `ghci Main.hs` を起動する |
| `make fmt` | fourmolu で整形する |
| `make fmt-check` | 整形済みか検査する |
| `make hooks` | pre-commit hook を有効化する |
| `make tags` | fast-tags で `tags` を生成する |
| `make hoogle` | ローカル Hoogle を http://localhost:8080 で起動する |
| `make clean` | `.build/` と `tags` を削除する |

## pre-commit hook

private repository なので CI は置かず、commit 時に `make fmt-check` と
`make check` を回す。作業ツリーではなく**ステージされた内容**を検査するので、
編集途中のファイルがあってもコミットは通る。

意図的に落としたいときは `git commit --no-verify`。

## パッケージを追加する

`flake.nix` の `ghcWithPackages (p: [ ... ])` にパッケージ名を足して `direnv reload`
(direnv を使わないなら `nix develop` に入り直す)。`build-depends` に相当する宣言は
どこにも要らず、そのまま import できる。
