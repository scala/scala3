scala.quoted
# class Liftable

## Companion object <a href="./Liftable$.md">Liftable</a>

<pre><code class="language-scala" >abstract class Liftable[T]</pre></code>
A typeclass for types that can be turned to `quoted.Expr[T]`
without going through an explicit `'{...}` operation.

## Constructors:
<pre><code class="language-scala" >Liftable()</pre></code>

## Concrete Value Members:
### toExpr
<pre><code class="language-scala" >def toExpr(x: <a href="./Liftable.md#T">T</a>): <a href="./Expr.md">Expr</a>[<a href="./Liftable.md#T">T</a>]</pre></code>

