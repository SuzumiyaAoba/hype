# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 言語/コミュニケーション方針

回答は日本語で簡潔かつ丁寧に行うこと。不明点や曖昧さは必ず確認し、無断で破壊的操作（リセット/クリーン等）は行わない。既存の未コミット変更は尊重し、巻き戻さない。

## プロジェクト概要

Hypeは静的型付け関数型言語で、Rust 2024 editionで実装されています。現在は最小サブセット段階で、主に式の型推論とJavaScriptへのトランスパイルを行います。

**技術スタック:**
- Rust 2024 edition
- 主要依存: clap, logos, rustyline, thiserror, anstyle
- テスト: insta (スナップショットテスト)

## コマンド

### 開発環境セットアップ

```bash
nix develop
```

Rust ツールチェーン（rustc/cargo/rustfmt/clippy 等）入りの dev shell を起動。

### ビルド・実行

```bash
# ビルド
cargo build

# 式を直接評価
cargo run -- "1 + 2 * 3"

# サンプルファイルの実行
cargo run -- "$(cat samples/simple.hp)"
cargo run -- "$(cat samples/complex.hp)"

# REPL起動（または引数なしで起動）
cargo run -- --repl
```

REPL では空行で入力確定、`:q` または `:quit` で終了。

### テスト

```bash
# 全テスト実行（スナップショット含む）
cargo test

# スナップショット更新が必要な場合
cargo test -- --nocapture  # スナップショットの差分を確認
cargo insta review         # insta による対話的レビュー
```

テストは `tests/` 配下に配置され、スナップショットテスト（insta）を多用しています。

### 型推論デバッグ

```bash
# 推論ログを標準出力に出力
cargo run -- --infer-debug-log - "let x = 42"

# 推論注釈付きソースを標準出力に出力
cargo run -- --infer-annotated-out - "let x = 42"

# または環境変数で指定
HYPE_INFER_LOG=- cargo run -- "let x = 42"
HYPE_INFER_ANNOTATED_OUT=- cargo run -- "let x = 42"
```

### Linting・フォーマット

```bash
# フォーマット
cargo fmt

# クリップチェック
cargo clippy
```

## コードベース構造

```
src/
├── main.rs         - CLI/REPLエントリポイント
├── lib.rs          - 公開API（transpile関数）
├── lexer.rs        - トークナイザ（logos使用）
├── parser.rs       - パーサー（再帰下降）
├── ast.rs          - AST定義（Expr, Stmt, Type, Pattern等）
├── typecheck.rs    - 型チェッカー（Hindley-Milner型推論）
├── render.rs       - JavaScriptコード生成
├── error.rs        - エラー型と整形
└── debug.rs        - 型推論デバッグ出力
tests/
├── snapshots/      - スナップショットテスト群
│   ├── arithmetic.rs
│   ├── bool_ops.rs
│   ├── match_blocks.rs
│   ├── hm_inference.rs
│   ├── infer_debug.rs
│   └── error_output.rs
└── type_errors.rs  - 型エラーテスト
samples/            - サンプルコード（.hp）
docs/
├── design.md       - 将来の言語設計メモ
└── syntax.md       - 文法（BNF）とサンプル
```

## 処理パイプライン

1. **字句解析** (`lexer::lex`) - ソースコードをトークン列に変換
2. **構文解析** (`parser::parse`) - トークン列をAST（`Vec<Stmt>`）に変換
3. **型検査** (`typecheck::typecheck`) - Hindley-Milner型推論で型チェック、環境（`TypeEnv`）を返す
4. **コード生成** (`render::render_program`) - ASTをJavaScriptに変換

デバッグモード時は型推論の詳細ログと推論済み型注釈を出力可能。

## 現在の言語機能（最小サブセット）

- 数値・文字列・真偽値リテラル
- 算術演算（`+`, `-`, `*`, `/`, `%`）と比較演算（`==`, `!=`, `<`, `>`, `<=`, `>=`）
- 論理演算（`&&`, `||`, `!`）
- let束縛（`let x = expr;` または `let x: Type = expr;`）
- let rec束縛（相互再帰可能）
- 関数定義（`fn name(params) { body }` または `fn name(params): Type { body }`）
- 無名関数（`\x -> expr` 構文）
- タプル型・リテラル・パターン（`(1, "a")`, `let (x, y) = ...`）
- リスト型・リテラル・パターン（`[1, 2, 3]`, `case [x, ...xs] => ...`）
- パターンマッチ（`match (expr) { case pattern => expr; ... }`）
- ブロック式（スコープ付き、最後の式が値）
- Hindley-Milner型推論（let多相含む、型変数と制約ベース）

**まだ未実装の機能（将来の拡張）:**
- 代数的データ型（`type Option<A> = None | Some { value: A }`）
- エフェクトシステム（`effect`, `handle`）
- 所有権・借用（`using` 構文）
- JSX風構文（`<div>...</div>`）
- モジュールシステム
- 型クラス
- その他 `docs/design.md` 記載の多数の高度な機能

詳細は `docs/syntax.md` と `docs/design.md` を参照。

## 型推論の仕組み

`typecheck.rs` で実装されている Hindley-Milner 型推論:

- 型変数（`Type::Var(TypeVarId)`）を生成し、制約を集める
- 単一化（unification）で制約を解決し、型代入（`Subst`）を構築
- let束縛時に型スキーム（`Scheme { vars, ty }`）を汎化し、let多相を実現
- 推論結果は `TypeEnv` に蓄積され、各式の型は `Expr` の `ty` フィールドに格納

型注釈は省略可能で、Number/String/Bool の範囲で推論が行われます。引数型は現状必須。

## GitHub Project連携

作業状況は GitHub Project「Kanban for AI」（#4）に反映すること（Backlog→In Progress→Done）。

```bash
# Project操作（gh CLI使用）
gh project item-add 4 --owner SuzumiyaAoba --url <issue-url>
gh project item-edit --id <item-id> --field-id <field-id> --project-id PVT_kwHOAmaFM84BKgQt

# チケット作成（Draft は使わない）
gh issue create --title "..." --body "..."
gh project item-add 4 --owner SuzumiyaAoba --url <issue-url>
```

project スコープ不足で失敗する場合はトークン権限を確認。

## コーディング方針

- 既存のスタイルに合わせる（ASCII コメント基本、必要最小限）
- 自動整形は `cargo fmt`、リンターは `cargo clippy` に従う
- 依存追加や大規模変更が必要な場合は事前に理由を明示
- テストは可能な限り実行し、スナップショットテストの更新は慎重に
- 手元検証手順があれば簡潔にメモ

## 追記方針

今後、ユーザ指示で覚えておくべき事項があれば AGENTS.md または本ファイルに追記して記録する。
