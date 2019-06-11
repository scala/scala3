scala.internal.quoted
# object Matcher

<pre><code class="language-scala" >final object Matcher extends Serializable</pre></code>
## Known subclasses:
<a href="./Matcher$/Matching$.md">Matching</a>
## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): Boolean</pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: Any): Boolean</pre></code>

### finalize
<pre><code class="language-scala" >protected def finalize(): Unit</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): Boolean</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

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

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

