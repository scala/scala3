scala.quoted.Expr$
# class AsContextualFunction

<pre><code class="language-scala" >class AsContextualFunction[F, Args <: scala.Tuple, R]</pre></code>
## Constructors:
<pre><code class="language-scala" >AsContextualFunction(f: <a href="../Expr.md">Expr</a>[F])(tf: TupledFunction[F, ImplicitFunction1[Args, R]])</pre></code>

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply[G](tg: TupledFunction[G, (<a href="#TupleOfExpr">TupleOfExpr</a>[<a href="./AsContextualFunction.md#Args">Args</a>]) => <a href="../Expr.md">Expr</a>[<a href="./AsContextualFunction.md#R">R</a>]]): G</pre></code>
Beta-reduces the function appication. Generates the an expression only containing the body of the function

