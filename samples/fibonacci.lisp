; Fibonacci function in Lisp syntax

(defn fib [n]
  (let [a 0
        b 1]
    (do
      (let [next (+ a b)])
      next)))

; Calculate fib(10)
(fib 10)
