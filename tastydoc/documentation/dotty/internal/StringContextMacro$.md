dotty.internal
# object StringContextMacro

<pre><code class="language-scala" >final object StringContextMacro extends Serializable</pre></code>
## Concrete Value Members:
### f
<pre><code class="language-scala" >inline def f(sc: => StringContext)(args: Seq[Any]): String</pre></code>
Implemetation of scala.StringContext.f used in Dotty while the standard library is still not bootstrapped

### inline$fImpl
<pre><code class="language-scala" >def inline$fImpl(sc: <a href="../../scala/quoted/Expr.md">Expr</a>[StringContext], args: <a href="../../scala/quoted/Expr.md">Expr</a>[Seq[Any]]): <a href="../../scala/quoted/Expr.md">Expr</a>[String]</pre></code>

