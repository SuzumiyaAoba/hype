# メンテナンスメモ

- 管理方針: GitHub Project「Kanban for AI」（Userプロジェクト、番号 #4、URL: https://github.com/users/SuzumiyaAoba/projects/4、Project ID: `PVT_kwHOAmaFM84BKgQt`）を使って言語の最低限の機能開発をトラッキングします（Backlog→In Progress→Done）。`gh` で操作する際は `--owner SuzumiyaAoba --number 4` を指定します。

- Backlog（最低限の言語機能）
  - Boolリテラル・比較/論理演算子のパース＋型チェック
  - if/else とブロック式（スコープ付）＋型チェック
  - 変数/関数スコープの未定義検出（現在は簡易）
  - エラーのスパン精度向上（現在は0..0が多い箇所を修正）
  - スナップショット/ユニットテスト追加（bool/if/比較）
  - REPLで複数文入力を許容（セミコロン処理の改善）
