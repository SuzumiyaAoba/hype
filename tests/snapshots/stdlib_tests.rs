use insta::assert_snapshot;

#[test]
fn test_option_type() {
    let code = r#"
;; Option型のテスト
(deftype Option [A]
  None
  (Some {:value A}))

(defn unwrap_or [opt default]
  (match opt
    [None default]
    [(Some {:value v}) v]))

(defn is_some [opt]
  (match opt
    [None false]
    [(Some {:value _}) true]))

;; テストケース
(let [x (Some {:value 42})]
  (let [y None]
    (let [z (unwrap_or x 0)]
      (let [w (unwrap_or y 99)]
        (+ z w)))))
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn test_result_type() {
    let code = r#"
;; Result型のテスト
(deftype Result [T E]
  (Ok {:value T})
  (Err {:error E}))

(defn is_ok [res]
  (match res
    [(Ok {:value _}) true]
    [(Err {:error _}) false]))

(defn unwrap_or_result [res default]
  (match res
    [(Ok {:value v}) v]
    [(Err {:error _}) default]))

;; テストケース
(let [success (Ok {:value 100})]
  (let [failure (Err {:error "oops"})]
    (if (is_ok success)
      (unwrap_or_result success 0)
      (unwrap_or_result failure 42))))
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn test_modulo_operator() {
    let code = r#"
;; モジュロ演算子のテスト
(defn is_even [n]
  (== (% n 2) 0))

(defn is_odd [n]
  (== (% n 2) 1))

(let [a (is_even 4)]
  (let [b (is_odd 5)]
    (and a b)))
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn test_list_fold() {
    let code = r#"
;; fold_left関数のテスト
(defn fold_left [f init lst]
  (match lst
    [[] init]
    [[x ...rest] (fold_left f (f init x) rest)]))

(let [add (fn [a b] (+ a b))]
  (fold_left add 0 [1 2 3 4 5]))
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn test_list_length() {
    let code = r#"
;; length関数のテスト（末尾再帰）
(defn length [lst acc]
  (match lst
    [[] acc]
    [[_ ...rest] (length rest (+ acc 1))]))

(length [1 2 3 4 5] 0)
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}
