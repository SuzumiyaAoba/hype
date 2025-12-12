# サンプルの実行方法

`samples/*.hype` に四則演算のサンプルを置いています。

実行例:

```sh
nix develop --command cargo run -- "$(cat samples/simple.hype)"
nix develop --command cargo run -- "$(cat samples/complex.hype)"
```

`cargo run -- "<expr>"` の形でも直接式を渡せます。
