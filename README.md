# Quasi-Affine Toy Compiler

**Objective:** Consume a quasi-affine expression, `expr`, and a constant, `c`, and then return `expr // c` and `expr % c` if these results are also quasi-affine.

## How to Run
Install `scala` and `sbt`, and then run:
```scala
sbt run
```

## Passes

Whenever we receive an expression, we first try to simplify it into a quasi-affine function.
If we can't do so successfully, we throw an exception (although the main program will catch those exceptions so that we can show samples of both valid and invalid expressions).

Some of the relevant optimizing passes we run are as follows:
* _Canonicalization:_ Modify all index variables to have a stride of 1 and to start at 0.
* _Const-fold floor divisions:_ If the maximum possible value of an expression is smaller than the denominator in our division expression, we simply replace it by a constant 0.
* _Const-fold modulo:_ If the stride of an index variable is identical to an outer modulo expression, we optimize the modulo expression to a compile-time constant.
* _Rewrite divisions that are nested by modulo operators:_ We replace expressions of the form `(a // c1) % c2` with `(a // c1) - c2 * (a // (c1*c2))`.
This allows us to rewrite a non-quasi-affine expression into a quasi-affine one.
* _Remove nested floor divisions:_ We replace expressions of the form `(a // c1) // c2` with expressions of the form `a // (c1 * c2)` where the denominator is a compile-time constant.
* _Remove nested modulo operators:_ We replace expressions of the form `(a % c1) % c2` with:
    - `a % c2` when `c1 % c2 == 0`, or
    - `a % c1` when `c2 % c1 == 0`
    
There are other passes as well, but these were the most relevant to mention.

## Testing

Better testing infrastructure needs to be added.
Right now, we use the `tests.py` script for testing, but this is manual and not scalable.
