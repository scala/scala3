scala.quoted
# object Expr

## Companion class <a href="./Expr.md">Expr</a>

<pre><code class="language-scala" >final object Expr extends Serializable</pre></code>
## Concrete Type Members:
### TupleOfExpr
<pre><code class="language-scala" >type TupleOfExpr: [Tup >: scala.Nothing <: scala.Tuple] => scala.Tuple.Map[Tup, [+T >: scala.Nothing <: scala.Any] => scala.quoted.Expr[+T]]</pre></code>
Converts a tuple `(T1, ..., Tn)` to `(Expr[T1], ..., Expr[Tn])`


### AsContextualFunction
<pre><code class="language-scala" >class <a href="./Expr$/AsContextualFunction.md">AsContextualFunction</a></pre></code>
### AsFunction
<pre><code class="language-scala" >class <a href="./Expr$/AsFunction.md">AsFunction</a></pre></code>
### ExprOps
<pre><code class="language-scala" >class <a href="./Expr$/ExprOps.md">ExprOps</a></pre></code>
