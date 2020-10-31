# Thoughts and notes on a potential simplifier

## ANF

As a very first step, we transform into ANF. This has two major benefits:

1.  It simplifies the analysis of which subexpressions might cause side effects, such as raising an exception or non-termination, at runtime. For instance, if we have an expression `f A1 ... An` where the `Ai` are all atomic expressions and we know that `f` won't cause any side effects when applied to `n` arguments, then we know that the whole expression `f A1 ... An` won't cause any side effects. If the `Ai` were complex expressions, we would need to analyze them for side effects as well.

1.  Every subexpression of the original program will have a name. This is useful for simplifications like applying record projections to know records. For instance, consider a program like
    ```
    let r = Record {field1 = E, ...};
    ...
    let x = r.field1;
    ...
    ```
    We would like to rewrite the `let x = r.field1;` line into `let x = E;`. However, if `E` was a complex expression, we would end up evaluating it twice (unless we perform perfect subexpression elimination). In contrast, if `E` is an atomic expression, then no evaluation will be performed by the `let x = E;` line. In fact, if `E` is a "small" atomic expression, such as a reference to a variable or an integer, the simplifier will remove this line entirely and replace all call sites of `x` with `E`.


## General structure

The simplifier performs symbolic evaluation while traversing the AST top-down and collects information about subexpressions while coming back bottom-up.

While traversing into the AST top-down, we store for each variable that has been bound so far a symbolic approximation of its value. These approximations of values are similar to WHNF in the sense that we store the outermost constructor and the _atomic_ expressions of its arguments. We also store partially applied primitives and lambdas in this way. There's also a special value `Abstract`, which indicates that we don't know anything useful about it. For instance, we don't know anything about function arguments or the results of applying primitives. We also store a list of variable renamings.

While coming back up the AST, we return information about the subexpression just handled, such as:

* an approximation of its value,
* a lower bound on its arity at runtime,
* whether evaluating the expression causes side effects.

In case of a `let`-binding `let x = A; B`, we use the approximation of `A`'s value when descending into `B`.

We use the lower bound on the arity of a function `f` to decide how to split the arguments in a call to `f`. For instance, if we don't know anything about the arity of `f`, then the ANF transformation has to turn `f A B C` into
```
let f1 = f A;
let f2 = f1 B;
f2 C
```
However, if we know that `f` has at least arity 2, we can transform into the more efficient
```
let f2 = f A B;
f2 C
```

We use the information whether an expression causes side effects to decide which dead `let`-bindings can be eliminated and which ones can't since eliminating them would remove potential side effects. This information would also become useful if we decided to reshuffle `let`-bindings. If `x` is not free in `B` and `y` not free in `A`, then we can rearrange `let x = A; let y = B;` into `let y = B; let x = A;` only if at most one of `A` and `B` has side effects.


## Inlining heuristics

We currently inline functions whenever they are fully applied. This is undesirable in general as it leads to code bloat. Beyond the general strategy of not inlining "large" functions, we could track what a function does to its arguments. For example, if a function pattern matches on one of its arguments and we know the constructor of the value we bind to that argument, then it more interesting to inline this function as it will save work at runtime. Similarly, it is more interesting to inline a higher-order function if we know that it will fully apply the function we pass in.

We will also need some form of "loop breakers" for recursive definitions. Our current thinking is to sort the definitions in a module topologically and flatten each recursive group in an arbitrary order. During inlining, a function would only be able to inline functions that come before it in this pseudo-topological ordering.


## Batching function applications

Assume we know that `f` has arity at least 3. Then we rewrite
```
let g = f A;
let h = g B;
let k = g C;
```
into
```
let g = f A;
let h = f A B;
let k = f A C;
```
If `g` is dead after this rewriting, we can eliminate it and have saved one iteration of the interpreter's main loop and one PAP allocation at the expense of having to apply one more _atomic_ argument. This is a good trade-off since pushing an atomic argument to is comparatively cheaper. (TODO: Quantify this.) However, if `g` is not dead after this rewriting, we've introduced the additional work of applying two more _atomic_ arguments. This is not great but at least it does not add any additional steps through the main loop of the interpreter or PAP allocations.
