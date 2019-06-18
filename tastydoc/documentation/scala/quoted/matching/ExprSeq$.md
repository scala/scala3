scala.quoted.matching
# object ExprSeq

<pre><code class="language-scala" >final object ExprSeq extends Serializable</pre></code>
Literal sequence of expressions

## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply[T](expr: <a href="../Expr.md">Expr</a>[Seq[T]])(implicit reflect: <a href="../../tasty/Reflection.md">Reflection</a>): Option[Seq[<a href="../Expr.md">Expr</a>[T]]]</pre></code>
Matches a literal sequence of expressions

