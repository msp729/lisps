# primitive lisp
given functions: SET, LAMBDA, MU, ATOM, EQ, CONS, CAR, CDR

- (SET X Y) means that in the future, the atom X can be replaced with Y. If X is not an atom, UB.
- (ATOM X) is the familiar predicate
- (EQ X Y) is only guaranteed to work for atoms
- CONS constructs a product
- CAR & CDR eliminate it
- COND is like in common lisp
- MU is for macros (not implemented yet)
- LAMBDA is for functions

```ebnf
<s-expr> ::= <atom> | "(" <s-expr>+ "." <s-expr> ")" | "(" <s-expr>* ")" | "'" <s-expr>
```

Equivalently,

```ebnf
<s-expr> ::= <atom> | "(" <lp> | "'" <s-expr>
<lp> = ")" | <s-expr> <lpse>
<lpse> = "." <s-expr> ")" | <lp>
```

This is how the parser works, after minimal lexing
