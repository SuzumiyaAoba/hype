; Type-annotated Lisp examples

; Function with type annotations
(defn add [x: Number y: Number] -> Number
  (+ x y))

; Function with partial type annotations
(defn greet [name: String]
  name)

; Lambda with type annotations
(fn [x: Number] -> Number
  (* x 2))

; Complex types
(defn map-add [xs: (List Number)] -> (List Number)
  xs)

; Multiple parameters with types
(defn calculate [a: Number b: Number c: Number] -> Number
  (+ (* a b) c))

; Type inference still works
(defn multiply [x y]
  (* x y))
