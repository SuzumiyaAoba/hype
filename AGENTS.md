# メンテナンスメモ

- このファイルの目的: エージェント向けの共通運用/開発指示の記録。

- エージェント運用指示
  - 回答は日本語で簡潔かつ丁寧に行うこと。
  - 不明点や曖昧さは必ず確認し、無断で破壊的操作（リセット/クリーン等）は行わない。
  - 既存の未コミット変更は尊重し、巻き戻さない。
  - 作業状況は常に GitHub Project「Kanban for AI」（#4）に反映すること（Backlog→In Progress→In review、マージはユーザ側で行う）。

- プロジェクト構造/技術スタック
  - Rust 2024 edition。主要依存: clap, logos, rustyline, thiserror, anstyle, insta（テスト）。
  - エントリ: `src/main.rs`（CLI/REPL）。コアロジック: `src/lib.rs`。統合テスト: `tests/`。サンプルコード: `samples/`。

- ビルド/テストの目安
  - ビルド: `cargo build`
  - テスト: `cargo test`（スナップショット含む）

- Nix/開発環境
  - GitHub CLI `gh` が利用可能。Project #4 操作時は `--owner SuzumiyaAoba --number 4` を付ける。project スコープ不足で失敗する場合はトークン権限を確認する。
  - `nix develop` で Rust ツールチェーン（rustc/cargo/rustfmt/clippy 等）入りの dev shell を利用可能。シェル起動時にバージョン表示の hook が走る。

- 開発/コード方針
  - 既存のスタイルに合わせ、必要最小限のコメントのみ追加する（ASCII を基本）。
  - 自動整形/リンターがある場合は従う。なければ周囲の書き方に合わせる。
  - 依存追加や大規模変更が必要な場合は事前に理由を明示する。

- テスト/検証
  - 可能なら関連テストを実行して結果を共有する。実行できない場合は理由とリスクを記載する。
  - 手元検証手順があれば簡潔にメモする。

- 管理方針: GitHub Project「Kanban for AI」（Userプロジェクト、番号 #4、URL: https://github.com/users/SuzumiyaAoba/projects/4、Project ID: `PVT_kwHOAmaFM84BKgQt`）を使って言語の最低限の機能開発をトラッキングします（Backlog→In Progress→In review）。`gh` で操作する際は `--owner SuzumiyaAoba --number 4` を指定します。project スコープ不足で `gh project ...` が失敗する場合は権限付与後に再実行する。
- チケット作成は Issue で行う（Draft は使わない）。`gh issue create ...` で発行し、`gh project item-add 4 --owner SuzumiyaAoba --url <issue>` でKanbanに登録する。
- 今後、ユーザ指示で覚えておくべき事項があれば本ファイルに追記して記録する。

- Backlog（最低限の言語機能）
  - Boolリテラル・比較/論理演算子のパース＋型チェック
  - if/else とブロック式（スコープ付）＋型チェック
  - 変数/関数スコープの未定義検出（現在は簡易）
  - エラーのスパン精度向上（現在は0..0が多い箇所を修正）
  - スナップショット/ユニットテスト追加（bool/if/比較）
  - REPLで複数文入力を許容（セミコロン処理の改善）
