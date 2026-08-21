# ADR 0001: Cabal project を作らず ghcWithPackages + Makefile で回す

- Status: Accepted
- Date: 2026-08-21

## Context

このリポジトリは Haskell の練習用で、成果物を配布する予定はない。
やりたいのは「気になったパッケージをとりあえず import して試す」ことで、
そのサイクルはできるだけ短いほうがいい。

Cabal project を作る場合、パッケージを1つ試すのに

1. `*.cabal` の `build-depends` を編集する
2. `cabal build` で依存を解決・ビルドする
3. HLS が新しい依存を認識するのを待つ

という手順を踏む。試して要らなかったら同じ手順を逆に辿る。
「気軽に試す」という目的に対してこの往復は重い。

一方 Nix の `haskellPackages.ghcWithPackages` は、指定したパッケージを
グローバルパッケージ DB に持つ GHC ラッパーを作る。
その GHC から見るとパッケージは `base` と同じ扱いになるので、
`ghc Main.hs` も `ghci` も、`build-depends` に相当する宣言なしで import できる。

## Decision

Cabal project (`*.cabal` / `cabal.project`) を作らない。

- 依存は `flake.nix` の `ghcWithPackages (p: [ ... ])` のリストで宣言する
- ビルドは `ghc` を直接呼ぶ。そのエントリポイントとして `Makefile` を置く
- 開発環境の出入りは direnv (`.envrc` の `use flake`) に任せる

パッケージを1つ試す手順は「`flake.nix` に1行足す → `direnv reload`」になる。

## Consequences

### 得られるもの

- 依存の追加・削除が1行の編集で済む
- `ghci` を起動した時点で対象パッケージが import できる
- HLS も同じ GHC を見るので、補完と型が追加直後から効く
- 依存の解決結果は `flake.lock` で固定される。`nix develop` すれば同じ環境が再現する

### 支払う代償

Cabal がやってくれていたことを自前で持つ必要がある。これが今回の変更の中身。

- **ビルド成果物の隔離**: Cabal は `dist-newstyle/` に成果物をまとめるが、
  `ghc` は素で呼ぶとソースの隣に `.hi` / `.o` を吐く。
  Makefile で `-outputdir .build` を指定して1箇所に押し込み、`.gitignore` で除外する。
  除外対象は `.build/` だけでなく `*.hi` / `*.o` / `*.dyn_*` / `*.hie` も入れてある
  (Makefile を経由せず素で `ghc Main.hs` した場合の取りこぼし対策)。
- **ビルドの依存追跡**: Makefile 側の依存は `$(wildcard *.hs)` という粗い粒度で、
  どれか1つ触れば ghc を呼び直す。実際にどのモジュールを再コンパイルするかの判断は
  ghc の recompilation checker に任せている。ファイル数が増えても破綻はしないが、
  サブディレクトリを掘るなら `-i` の追加が必要になる。
- **パッケージのバージョン指定ができない**: 使えるのは nixpkgs の `haskellPackages` が
  持っているバージョンだけ。特定バージョンを狙うなら overlay を書くことになり、
  それは Cabal + `cabal.project` の constraints より面倒。
- **テストとベンチのハーネスが無い**: `cabal test` に相当するものは無い。
  必要になったら Makefile にターゲットを足す。
- **配布可能な成果物ではない**: ライブラリとして公開する、あるいは他のプロジェクトから
  依存させる段になったら Cabal 化は避けられない。そのときは移行する。

## Alternatives considered

- **Cabal project を普通に作る**: 正攻法で、上記の代償は全部消える。
  却下した理由は Context の通り、試行のサイクルが重くなるため。
  配布や CI が視野に入ったら再検討する。
- **cabal script** (ファイル先頭に `{- cabal: build-depends: ... -}` を書く単一ファイル形式):
  Cabal project を作らずに依存を宣言できる点で目的に近い。
  ただし依存の宣言がファイルごとに閉じるのでファイルを分けると破綻し、
  HLS のサポートも Cabal project ほど厚くない。
- **Stack**: Stackage のスナップショットでバージョンが揃うのは利点だが、
  Nix と役割が重複する。環境の固定は flake.lock に一本化したい。
