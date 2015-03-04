# cool-relational-interpreter-examples
Fun, interesting, and thought-provoking examples of relational Scheme interpreters at work.

All examples run under Vicare Scheme.  OS X users may be interested in Michael Ballantyne's Homebrew formula for Vicare:

https://github.com/michaelballantyne/homebrew-vicare

I'm working with Michael Ballantyne to find/create additional interesting examples.  Michael suggested running the Scheme definition of 'append' as a relation, which has led to many interesting examples.

The examples in `append/variadic-lambda-tests.scm` concentrate on uses of `append` (list concatenation) in a relational Scheme interpreter supporting variadic functions, `apply`, `letrec`, `list`, `car`, `cdr`, `cons`, and other built-ins.  The examples show how `append` can be implemented using the normal Scheme definition, but treated as a relation, since the interpreter itself is a relation.  The resulting relational behavior of `append` is strictly more general than that of the `appendo` goal often shown in miniKanren tutorials.