scala.tasty.reflect.QuotedOps
# class QuotedExprAPI

<pre><code class="language-scala" >class QuotedExprAPI[T]</pre></code>
## Constructors:
<pre><code class="language-scala" >QuotedExprAPI(expr: <a href="../../../quoted/Expr.md">Expr</a>[T])</pre></code>

## Concrete Value Members:
### cast
<pre><code class="language-scala" >def cast[U](implicit evidence$17: <a href="../../../quoted/Type.md">Type</a>[U], ctx: Context): <a href="../../../quoted/Expr.md">Expr</a>[U]</pre></code>
Checked cast to a `quoted.Expr[U]`

### show
<pre><code class="language-scala" >def show(ctx: Context): String</pre></code>
Show a source code like representation of this expression

### unseal
<pre><code class="language-scala" >def unseal(ctx: Context): Term</pre></code>
View this expression `quoted.Expr[T]` as a `Term`

