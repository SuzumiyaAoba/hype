use insta::assert_snapshot;

#[test]
fn tail_recursive_factorial() {
    let code = r#"
(defn factorial [n: Number acc: Number] -> Number
  (if (== n 0)
    acc
    (factorial (- n 1) (* n acc))))

(factorial 5 1)
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn tail_recursive_sum() {
    let code = r#"
(defn sum_to [n: Number acc: Number] -> Number
  (if (== n 0)
    acc
    (sum_to (- n 1) (+ acc n))))

(sum_to 10 0)
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn non_tail_recursive_factorial() {
    // This should NOT be optimized - the recursive call is not in tail position
    let code = r#"
(defn factorial_bad [n: Number] -> Number
  (if (== n 0)
    1
    (* n (factorial_bad (- n 1)))))

(factorial_bad 5)
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn tail_recursive_with_match() {
    let code = r#"
(defn countdown [n: Number] -> Number
  (match n
    [0 0]
    [x (countdown (- x 1))]))

(countdown 10)
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn tail_recursive_list_length() {
    let code = r#"
(defn list_len [xs: (List Number) acc: Number] -> Number
  (match xs
    [[] acc]
    [[x ...rest] (list_len rest (+ acc 1))]))

(list_len [1 2 3 4 5] 0)
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}

#[test]
fn non_tail_call_in_arguments() {
    // Recursive call in argument position - NOT tail recursive
    let code = r#"
(defn add_then_recurse [n: Number] -> Number
  (if (== n 0)
    0
    (+ 1 (add_then_recurse (- n 1)))))

(add_then_recurse 5)
"#;

    let js = hype::transpile(code).unwrap();
    assert_snapshot!(js);
}
