scala.quoted.Expr$
# class AsFunction

<pre><code class="language-scala" >class AsFunction[F, Args <: scala.Tuple, R]</pre></code>
## Constructors:
<pre><code class="language-scala" >AsFunction(f: <a href="../Expr.md">Expr</a>[F])(tf: TupledFunction[F, (Args) => R])</pre></code>

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply[G](tg: TupledFunction[G, (<a href="#TupleOfExpr">TupleOfExpr</a>[<a href="./AsFunction.md#Args">Args</a>]) => <a href="../Expr.md">Expr</a>[<a href="./AsFunction.md#R">R</a>]]): G</pre></code>
Beta-reduces the function appication. Generates the an expression only containing the body of the function

