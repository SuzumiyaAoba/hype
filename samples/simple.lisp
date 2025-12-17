; Simple Lisp syntax examples

; Basic arithmetic
(+ 1 2)

; Nested expressions
(+ (* 2 3) 4)

; Let binding
(let [x 10
      y 20]
  (+ x y))

; Function definition
(defn add [x y]
  (+ x y))

; Function call
(add 5 3)

; Lambda
(fn [x] (* x 2))

; Boolean operations
(and true false)
(or true false)
(< 1 2)

; List literal
[1 2 3 4 5]

; String literal
"Hello, Lisp!"
