# cool-relational-interpreter-examples
Fun, interesting, and thought-provoking examples of relational Scheme interpreters at work.

I'm working with Michael Ballantyne to find/create additional interesting examples.  Michael suggested running the Scheme definition of `append` as a relation, which has led to many interesting examples.

---

The miniKanren implementation is a git submodule.  Once you clone this repo, you can get the miniKanren implementation by running:

```
git submodule init
git submodule update
```

All examples run under Vicare Scheme and Petite Chez Scheme.  OS X users may be interested in Michael Ballantyne's Homebrew formula for Vicare:

https://github.com/michaelballantyne/homebrew-vicare

---

The examples in `append/variadic-lambda-tests.scm` concentrate on uses of `append` (list concatenation) in a relational Scheme interpreter supporting variadic functions, `apply`, `letrec`, `list`, `car`, `cdr`, `cons`, and other built-ins.  The examples show how `append` can be implemented using the normal Scheme definition, but treated as a relation, since the interpreter itself is a relation.  The resulting relational behavior of `append` is strictly more general than that of the `appendo` goal often shown in miniKanren tutorials.

---

The examples in `unspecified-behavior/interp-curried-two-directions-tests.scm` show how we can infer Scheme expressions whose values differ under left-to-right and right-to-left evaluation orders.  The inferred expressions return different values under Vicare Scheme, Petite Chez Scheme, and Racket.

---
