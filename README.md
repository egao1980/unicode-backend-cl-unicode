# unicode-backend-cl-unicode

[`unicode-protocol`](https://github.com/egao1980/unicode-protocol) backend over **[cl-unicode](https://github.com/edicl/cl-unicode)**.

Implements `:properties` `:normalize` `:casefold` `:idna` `:script` `:char-name`.

IDNA/UTS#46 algorithm lives here (ported from `cl-idna`); Ultralisp **`egao1980/cl-idna` is not modified**. Stack apps should use [`cl-stack-idna`](https://github.com/egao1980/cl-stack-idna).

```lisp
(asdf:load-system "unicode-backend-cl-unicode")
(stack-unicode:idna-name-to-ascii "bücher.de")
```

## License

MIT
