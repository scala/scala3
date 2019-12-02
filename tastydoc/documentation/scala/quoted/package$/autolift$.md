scala.quoted.package$
# object autolift

<pre><code class="language-scala" >final object autolift extends Serializable</pre></code>
## Concrete Value Members:
### autoToExpr
<pre><code class="language-scala" >implicit def autoToExpr[T](x: T)(implicit evidence$21: <a href="../Liftable.md">Liftable</a>[T]): <a href="../Expr.md">Expr</a>[T]</pre></code>

