---
layout: singlepage-overview
scala3: true
title: "Macros Spec"
---

## Implementation

### Syntax

Compared to the [Scala 3 reference grammar](../syntax.html)
there are the following syntax changes:
```
SimpleExpr      ::=  ...
                  |  ‘'’ ‘{’ Block ‘}’
                  |  ‘'’ ‘[’ Type ‘]’
                  |  ‘$’ ‘{’ Block ‘}’
SimpleType      ::=  ...
                  |  ‘$’ ‘{’ Block ‘}’
```
In addition, an identifier `$x` starting with a `$` that appears inside
a quoted expression or type is treated as a splice `${x}` and a quoted identifier
`'x` that appears inside a splice is treated as a quote `'{x}`

### Implementation in `scalac`

Quotes and splices are primitive forms in the generated abstract syntax trees.
Top-level splices are eliminated during macro expansion while typing. On the
other hand, top-level quotes are eliminated in an expansion phase `PickleQuotes`
phase (after typing and pickling). PCP checking occurs while preparing the RHS
of an inline method for top-level splices and in the `Staging` phase (after
typing and before pickling).

Macro-expansion works outside-in. If the outermost scope is a splice,
the spliced AST will be evaluated in an interpreter. A call to a
previously compiled method can be implemented as a reflective call to
that method. With the restrictions on splices that are currently in
place that’s all that’s needed. We might allow more interpretation in
splices in the future, which would allow us to loosen the
restriction.  Quotes in spliced, interpreted code are kept as they
are, after splices nested in the quotes are expanded.

If the outermost scope is a quote, we need to generate code that
constructs the quoted tree at run-time. We implement this by
serializing the tree as a TASTy structure, which is stored
in a string literal. At runtime, an unpickler method is called to
deserialize the string into a tree.

Splices inside quoted code insert the spliced tree as is, after
expanding any quotes in the spliced code recursively.

## Formalization

The phase consistency principle can be formalized in a calculus that
extends simply-typed lambda calculus with quotes and splices.

### Syntax

The syntax of terms, values, and types is given as follows:
```
Terms         t  ::=  x                 variable
                      (x: T) => t       lambda
                      t t               application
                      't                quote
                      $t                splice

Values        v  ::=  (x: T) => t       lambda
                      'u                quote

Simple terms  u  ::=  x  |  (x: T) => u  |  u u  |  't

Types         T  ::=  A                 base type
                      T -> T            function type
                      expr T            quoted
```
Typing rules are formulated using a stack of environments
`Es`. Individual environments `E` consist as usual of variable
bindings `x: T`. Environments can be combined using the two
combinators `'` and `$`.
```
Environment   E  ::=  ()                empty
                      E, x: T

Env. stack    Es ::=  ()                empty
                      E                 simple
                      Es * Es           combined

Separator     *  ::=  '
                      $
```
The two environment combinators are both associative with left and
right identity `()`.

### Operational semantics

We define a small step reduction relation `-->` with the following rules:
```
          ((x: T) => t) v  -->  [x := v]t

                    ${'u}  -->  u

                       t1  -->  t2
                    -----------------
                    e[t1]  -->  e[t2]
```
The first rule is standard call-by-value beta-reduction. The second
rule says that splice and quotes cancel each other out. The third rule
is a context rule; it says that reduction is allowed in the hole `[ ]`
position of an evaluation context.  Evaluation contexts `e` and
splice evaluation context `e_s` are defined syntactically as follows:
```
Eval context    e    ::=  [ ]  |  e t  |  v e  |  'e_s[${e}]
Splice context  e_s  ::=  [ ]  |  (x: T) => e_s  |  e_s t  |  u e_s
```

### Typing rules

Typing judgments are of the form `Es |- t: T`. There are two
substructural rules which express the fact that quotes and splices
cancel each other out:
```
                    Es1 * Es2 |- t: T
               ---------------------------
               Es1 $ E1 ' E2 * Es2 |- t: T


                    Es1 * Es2 |- t: T
               ---------------------------
               Es1 ' E1 $ E2 * Es2 |- t: T
```
The lambda calculus fragment of the rules is standard, except that we
use a stack of environments. The rules only interact with the topmost
environment of the stack.
```
                          x: T in E
                        --------------
                        Es * E |- x: T


                    Es * E, x: T1 |- t: T2
                -------------------------------
                Es * E |- (x: T1) => t: T -> T2


               Es |- t1: T2 -> T    Es |- t2: T2
               ---------------------------------
                      Es |- t1 t2: T
```
The rules for quotes and splices map between `expr T` and `T` by trading `'` and `$` between
environments and terms.
```
                     Es $ () |- t: expr T
                     --------------------
                         Es |- $t: T


                       Es ' () |- t: T
                       ----------------
                       Es |- 't: expr T
```
The meta theory of a slightly simplified 2-stage variant of this calculus
is studied [separately](./simple-smp.html).

## Going Further

The metaprogramming framework as presented and currently implemented is quite restrictive
in that it does not allow for the inspection of quoted expressions and
types. It’s possible to work around this by providing all necessary
information as normal, unquoted inline parameters. But we would gain
more flexibility by allowing for the inspection of quoted code with
pattern matching. This opens new possibilities.

For instance, here is a version of `power` that generates the multiplications
directly if the exponent is statically known and falls back to the dynamic
implementation of `power` otherwise.
```scala
import scala.quoted.*

inline def power(x: Double, n: Int): Double =
  ${ powerExpr('x, 'n) }

private def powerExpr(x: Expr[Double], n: Expr[Int])
                     (using Quotes): Expr[Double] =
  n.value match
    case Some(m) => powerExpr(x, m)
    case _ => '{ dynamicPower($x, $n) }

private def powerExpr(x: Expr[Double], n: Int)
                     (using Quotes): Expr[Double] =
  if n == 0 then '{ 1.0 }
  else if n == 1 then x
  else if n % 2 == 0 then '{ val y = $x * $x; ${ powerExpr('y, n / 2) } }
  else '{ $x * ${ powerExpr(x, n - 1) } }

private def dynamicPower(x: Double, n: Int): Double =
  if n == 0 then 1.0
  else if n % 2 == 0 then dynamicPower(x * x, n / 2)
  else x * dynamicPower(x, n - 1)
```

In the above, the method `.value` maps a constant expression of the type
`Expr[T]` to its value of the type `T`.

With the right extractors, the "AsFunction" conversion
that maps expressions over functions to functions over expressions can
be implemented in user code:
```scala
given AsFunction1[T, U]: Conversion[Expr[T => U], Expr[T] => Expr[U]] with
  def apply(f: Expr[T => U]): Expr[T] => Expr[U] =
    (x: Expr[T]) => f match
      case Lambda(g) => g(x)
      case _ => '{ ($f)($x) }
```
This assumes an extractor
```scala
object Lambda:
  def unapply[T, U](x: Expr[T => U]): Option[Expr[T] => Expr[U]]
```
Once we allow inspection of code via extractors, it’s tempting to also
add constructors that create typed trees directly without going
through quotes. Most likely, those constructors would work over `Expr`
types which lack a known type argument. For instance, an `Apply`
constructor could be typed as follows:
```scala
def Apply(fn: Expr[Any], args: List[Expr[Any]]): Expr[Any]
```
This would allow constructing applications from lists of arguments
without having to match the arguments one-by-one with the
corresponding formal parameter types of the function. We then need "at
the end" a method to convert an `Expr[Any]` to an `Expr[T]` where `T` is
given from the outside. For instance, if `code` yields a `Expr[Any]`, then
`code.atType[T]` yields an `Expr[T]`. The `atType` method has to be
implemented as a primitive; it would check that the computed type
structure of `Expr` is a subtype of the type structure representing
`T`.

Before going down that route, we should evaluate in detail the tradeoffs it
presents. Constructing trees that are only verified _a posteriori_
to be type correct loses a lot of guidance for constructing the right
trees.  So we should wait with this addition until we have more
use-cases that help us decide whether the loss in type-safety is worth
the gain in flexibility. In this context, it seems that deconstructing types is
less error-prone than deconstructing terms, so one might also
envisage a solution that allows the former but not the latter.

## Conclusion

Metaprogramming has a reputation of being difficult and confusing.
But with explicit `Expr/Type` types and quotes and splices it can become
downright pleasant. A simple strategy first defines the underlying quoted or unquoted
values using `Expr` and `Type` and then inserts quotes and splices to make the types
line up. Phase consistency is at the same time a great guideline
where to insert a splice or a quote and a vital sanity check that
the result makes sense.
