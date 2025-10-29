# An example STLC implementation

You could implement your own little language when you have learned Haskell!

Many parts of this implementation is simplified to reduce extra knowledge needed. It is NOT efficient, and some definitions are NOT concise as to functional programming standards.

There are some things to try out, varying in difficulty:
- Add comments to the language
- Add a `let` or other constructs
- Define a fold over terms, and express all semantics with that fold
- Implement some other semantics, for example lazy evaluation or evaluation with environments and closures
- Introduce side effects to the language, for example with a `print` construct
- Use the `megaparsec` parser combinator library to build a faster parser
- Try out higher-order abstract syntax or the tagless final style
- Implement type inference and type variables
