(import "console.ffi.lisp")

(defn greet [name]
  (do
    (log (+ (+ "Hello, " name) "!"))
    name))

(greet "World")
