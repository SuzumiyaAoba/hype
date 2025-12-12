# 文法（BNF）とサンプルコード

Haskell 風よりも馴染みやすい C/JS 系の記法に寄せた最小サブセットです。

## コア文法（最小サブセット BNF）
```
<module>       ::= { <topdecl> }
<topdecl>      ::= <type-decl> | <fn-decl> | <let-decl> | <effect-decl> | <op-decl>

<type-decl>    ::= "type" <UIdent> [ "<" <type-params> ">" ] "=" <variant> { "|" <variant> }
<type-params>  ::= <UIdent> { "," <UIdent> }
<variant>      ::= <UIdent> [ "{" <field> { "," <field> } "}" ]
<field>        ::= <LIdent> ":" <type>

<fn-decl>      ::= "fn" <LIdent> "(" [<param-list>] ")" [":" <type>] "{" <stmt-list> "}"
<let-decl>     ::= "let" <LIdent> [":" <type>] "=" <expr> ";"
<effect-decl>  ::= "effect" <UIdent> "(" <type> ")" ":" <type> ";"
<op-decl>      ::= "infix" <prec> <assoc> <symbol> ";"

<param-list>   ::= <param> { "," <param> }
<param>        ::= <LIdent> [":" <type>]

<stmt-list>    ::= { <stmt> }
<stmt>         ::= <let-decl> | <expr> ";"

<type>         ::= <type-atom> | <type> "->" <type>
<type-atom>    ::= <UIdent>
                 | <UIdent> "<" <type> { "," <type> } ">"
                 | "{" <field> { "," <field> } "}"
                 | "(" <type> ")"

<expr>         ::= <assignment>
<assignment>   ::= <logic> | <LIdent> "=" <expr>
<logic>        ::= <term> | <term> <logic-op> <logic>
<logic-op>     ::= "&&" | "||"
<term>         ::= <factor> | <factor> <add-op> <term>
<add-op>       ::= "+" | "-"
<factor>       ::= <unary> | <unary> <mul-op> <factor>
<mul-op>       ::= "*" | "/" | "%"
<unary>        ::= <primary> | ("!" | "-") <primary>
<primary>      ::= <LIdent> | <UIdent> | <lit>
                 | <primary> "." <LIdent>
                 | <primary> "(" [<arg-list>] ")"
                 | "(" <expr> ")"
                 | <match>
                 | <handle>
                 | <using>
                 | <lambda>
                 | <record-expr>
<arg-list>     ::= <expr> { "," <expr> }
<record-expr>  ::= "{" <field-expr> { "," <field-expr> } "}"
<field-expr>   ::= <LIdent> ":" <expr>

<lambda>       ::= "fn" "(" [<param-list>] ")" "=>" <expr>
<match>        ::= "match" "(" <expr> ")" "{" <match-arm> { <match-arm> } "}"
<match-arm>    ::= "case" <pattern> "=>" <expr> ";"
<pattern>      ::= <LIdent> | "_" | <UIdent> [ "{" <pat-field> { "," <pat-field> } "}" ]
<pat-field>    ::= <LIdent> ":" <pattern>
<handle>       ::= "handle" <expr> "with" "{" <handler-arm> { <handler-arm> } "}"
<handler-arm>  ::= "case" <UIdent> "(" <LIdent> ")" "=>" <expr> ";"
<using>        ::= "using" <pattern> "=" <expr> "in" <expr>

<lit>          ::= <int> | <string> | "true" | "false"
<prec>         ::= <int>
<assoc>        ::= "left" | "right" | "nonassoc"
<symbol>       ::= <symbol-char> { <symbol-char> }
<symbol-char>  ::= "+" | "-" | "*" | "/" | "<" | ">" | "=" | "&" | "|" | "!" | "?"

<LIdent>       ::= <lower> { <letter> | <digit> | "_" }
<UIdent>       ::= <upper> { <letter> | <digit> | "_" }
```

## サンプルコード

### 1. 基本: 代数的データ型とパターンマッチ
```hype
type Option<A> = None | Some { value: A }

fn mapOption<A, B>(f: fn(A) -> B, opt: Option<A>): Option<B> {
  match (opt) {
    case None => None;
    case Some { value } => Some { value: f(value) };
  }
}
```

### 2. エフェクトとハンドラ（IO風の抽象）
```hype
effect Print(String): Unit;

fn greet(name: String): String {
  Print("Hello, " + name);
  return "done";
}

fn main(): Unit {
  handle greet("world") with {
    case Print(msg) => {
      console.log(msg);
      ();
    };
  }
}
```

### 3. 所有権と using による決定的解放
```hype
type File = File { fd: Fd }
effect Open(String): File;
effect Close(File): Unit;
effect Read(File): String;

fn readOnce(path: String): String {
  using file = Open(path) in {
    let content = Read(file);
    content
  }
}
```

### 4. JSX風構文（型付きテンプレート）
```hype
type Msg = Click | Input { value: String }

fn view(name: String) => 
  <div class="card">
    <h1>Hello, {name}</h1>
    <button onClick={_ => Click}>Tap</button>
    <input value={name} onInput={e => Input { value: e.target.value }} />
  </div>;
```
