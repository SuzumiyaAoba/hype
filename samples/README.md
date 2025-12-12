# サンプルの実行方法

`samples/*.hp` に四則演算のサンプルを置いています。

実行例:

```sh
nix develop --command cargo run -- "$(cat samples/simple.hp)"
nix develop --command cargo run -- "$(cat samples/complex.hp)"
```

`cargo run -- "<expr>"` の形でも直接式を渡せます。
