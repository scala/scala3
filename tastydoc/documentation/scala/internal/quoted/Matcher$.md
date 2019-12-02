scala.internal.quoted
# object Matcher

<pre><code class="language-scala" >final object Matcher extends Serializable</pre></code>
## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply[Tup <: scala.Tuple](scrutineeExpr: <a href="../../quoted/Expr.md">Expr</a>[Nothing <: Any])(implicit patternExpr: <a href="../../quoted/Expr.md">Expr</a>[Nothing <: Any], reflection: <a href="../../tasty/Reflection.md">Reflection</a>): Option[Tup]</pre></code>
Pattern matches an the scrutineeExpr aquainsnt the patternExpr and returns a tuple
with the matched holes if successful.
Examples:
* `Matcher.unapply('{ f(0, myInt) })('{ f(0, myInt) }, _)`
     will return `Some(())` (where `()` is a tuple of arity 0)
* `Matcher.unapply('{ f(0, myInt) })('{ f(patternHole[Int], patternHole[Int]) }, _)`
     will return `Some(Tuple2('{0}, '{ myInt }))`
* `Matcher.unapply('{ f(0, "abc") })('{ f(0, patternHole[Int]) }, _)`
     will return `None` due to the missmatch of types in the hole

Holes:
* scala.internal.Quoted.patternHole[T]: hole that matches an expression `x` of type `Expr[U]`
                                          if `U <:< T` and returns `x` as part of the match.

***return*** None if it did not match, `Some(tup)` if it matched where `tup` contains `Expr[Ti]```

***patternExpr*** `Expr[_]` containing the pattern tree

***reflection*** instance of the reflection API (implicitly provided by the macro)

***scrutineeExpr*** `Expr[_]` on which we are pattern matching

