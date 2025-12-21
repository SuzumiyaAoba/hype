# Hypeでのテスト

このドキュメントでは、Hypeプログラムのテスト方法について説明します。

## 標準テストライブラリ

Hypeは`std/test.ffi.hp`モジュールでテストサポート機能を提供します。このモジュールはJavaScriptの機能を活用したFFI（Foreign Function Interface）モジュールです。

### インポート

```lisp
(import "std/test.ffi.hp")
(import "std/console.ffi.hp")
```

## 利用可能な関数

### アサーション関数

#### `assert`
真偽値のアサーション。条件が偽の場合にエラーをthrowします。

```lisp
(assert true)   ; 成功
(assert false)  ; エラー: Assertion failed
```

**型シグネチャ**: `(-> Bool Unit)`

#### `assert_eq`
数値の等価性チェック。2つの数値が等しくない場合にエラーをthrowします。

```lisp
(assert_eq (+ 1 2) 3)      ; 成功
(assert_eq (* 2 3) 5)      ; エラー: Assertion failed: expected 5, got 6
```

**型シグネチャ**: `(-> Number Number Unit)`

#### `assert_eq_str`
文字列の等価性チェック。2つの文字列が等しくない場合にエラーをthrowします。

```lisp
(assert_eq_str "hello" "hello")  ; 成功
(assert_eq_str "hello" "world")  ; エラー: Assertion failed: expected 'world', got 'hello'
```

**型シグネチャ**: `(-> String String Unit)`

#### `assert_eq_bool`
真偽値の等価性チェック。2つの真偽値が等しくない場合にエラーをthrowします。

```lisp
(assert_eq_bool true true)    ; 成功
(assert_eq_bool true false)   ; エラー: Assertion failed: expected false, got true
```

**型シグネチャ**: `(-> Bool Bool Unit)`

### デバッグ関数

#### `debug`
デバッグメッセージを出力します。

```lisp
(debug "Debug message")  ; 出力: [DEBUG] Debug message
```

**型シグネチャ**: `(-> String Unit)`

### テスト結果出力関数

#### `test_pass`
テスト成功メッセージを出力します。

```lisp
(test_pass "Addition tests passed")  ; 出力: ✓ Addition tests passed
```

**型シグネチャ**: `(-> String Unit)`

#### `test_fail`
テスト失敗メッセージを出力します（標準エラー出力）。

```lisp
(test_fail "Subtraction tests failed")  ; 出力: ✗ Subtraction tests failed
```

**型シグネチャ**: `(-> String Unit)`

## 使用例

### 基本的なテスト

```lisp
(import "std/test.ffi.hp")
(import "std/console.ffi.hp")

(defn test-math []
  (do
    (log "Testing math operations...")
    (assert_eq (+ 1 2) 3)
    (assert_eq (* 2 3) 6)
    (assert_eq (- 10 5) 5)
    (test_pass "Math tests passed")))

(test-math)
```

### 複数のテストケース

```lisp
(import "std/test.ffi.hp")
(import "std/console.ffi.hp")

(defn test-addition []
  (do
    (log "Testing addition...")
    (assert_eq (+ 1 2) 3)
    (assert_eq (+ 0 0) 0)
    (assert_eq (+ -1 1) 0)
    (test_pass "Addition tests")))

(defn test-multiplication []
  (do
    (log "Testing multiplication...")
    (assert_eq (* 2 3) 6)
    (assert_eq (* 0 5) 0)
    (assert_eq (* -1 -1) 1)
    (test_pass "Multiplication tests")))

(defn test-all []
  (do
    (test-addition)
    (test-multiplication)
    (log "All tests passed!")))

(test-all)
```

### 文字列と真偽値のテスト

```lisp
(import "std/test.ffi.hp")
(import "std/console.ffi.hp")

(defn test-strings []
  (do
    (log "Testing strings...")
    (assert_eq_str "hello" "hello")
    (assert_eq_str (+ "hello" " world") "hello world")
    (test_pass "String tests")))

(defn test-booleans []
  (do
    (log "Testing booleans...")
    (assert_eq_bool (== 1 1) true)
    (assert_eq_bool (> 5 10) false)
    (assert true)
    (test_pass "Boolean tests")))

(test-strings)
(test-booleans)
```

## テストの実行

### コマンドラインから実行

Hypeコードをトランスパイルして、Node.jsで実行します。

```bash
# 直接実行
hype my_test.hp | node

# ファイルに出力してから実行
hype my_test.hp > test.js
node test.js
```

### テストファイルの命名規則

テストファイルには`_test.hp`という接尾辞をつけることを推奨します。

```
my_module.hp
my_module_test.hp
```

## 標準ライブラリパス

`std/`プレフィックスを使用すると、標準ライブラリから自動的にモジュールを解決します。

### カスタムパス

環境変数`HYPE_STD_PATH`を設定することで、標準ライブラリのパスをカスタマイズできます。

```bash
export HYPE_STD_PATH=/path/to/custom/std
hype my_test.hp | node
```

### デフォルトパス

`HYPE_STD_PATH`が設定されていない場合、以下のパスが使用されます：
- `<実行ファイルディレクトリ>/../lib/hype/std/`
- フォールバック: `lib/hype/std/`

## ベストプラクティス

### 1. テストを関数にまとめる

```lisp
(defn test-feature []
  (do
    (log "Testing feature...")
    (assert_eq (feature 1) 2)
    (test_pass "Feature tests")))
```

### 2. わかりやすいメッセージを使う

```lisp
(log "Testing edge cases for division...")
(assert_eq (/ 10 2) 5)
(test_pass "Division edge cases")
```

### 3. テストを階層化する

```lisp
(defn test-arithmetic []
  (do
    (test-addition)
    (test-subtraction)
    (test-multiplication)
    (test-division)))
```

### 4. デバッグ出力を活用する

```lisp
(defn test-complex-calculation []
  (do
    (let [result (+ (* 2 3) 4)]
      (do
        (debug "Intermediate result calculated")
        (assert_eq result 10)
        (test_pass "Complex calculation")))))
```

## 制限事項

### 型ごとのアサーション関数

現在のHypeの型システムでは、ジェネリック型変数がサポートされていないため、型ごとに異なるアサーション関数を使用する必要があります。

- 数値の比較: `assert_eq`
- 文字列の比較: `assert_eq_str`
- 真偽値の比較: `assert_eq_bool`

### テストランナー

現在、Hypeには専用のテストランナーは含まれていません。将来的に以下のような機能を持つテストランナーが追加される可能性があります：

- `*_test.hp`ファイルの自動検出
- テスト結果の集約
- TAP/JUnit形式での出力

## 将来の拡張

### 予定されている機能

- モック関数のサポート
- テストカバレッジツール
- ベンチマークツール
- パラメータ化テスト

## まとめ

Hypeのテスト機能は、以下の原則に基づいて設計されています：

1. **シンプル**: 基本的なアサーション関数のみを提供
2. **FFIベース**: JavaScriptの機能を最大限に活用
3. **型安全**: Hypeの型システムと統合
4. **拡張可能**: ユーザーが独自のテストユーティリティを追加可能

より高度なテスト機能が必要な場合は、独自のFFIモジュールを作成するか、JavaScript側でテストフレームワークを統合することができます。
