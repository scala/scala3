scala.quoted
# object Expr

## Companion class <a href="./Expr.md">Expr</a>

<pre><code class="language-scala" >final object Expr extends Serializable</pre></code>
## Known subclasses:
<a href="./Expr$/AsContextualFunction.md">AsContextualFunction</a>, <a href="./Expr$/AsFunction.md">AsFunction</a>, <a href="./Expr$/ExprOps.md">ExprOps</a>
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
## Concrete Value Members:
### AsContextualFunction
<pre><code class="language-scala" >final implicit def AsContextualFunction[F, Args <: scala.Tuple, R](f: <a href="./Expr.md">Expr</a>[F])(tf: TupledFunction[F, ImplicitFunction1[Args, R]]): <a href="./Expr$/AsContextualFunction.md">AsContextualFunction</a>[F, Args, R]</pre></code>

### AsFunction
<pre><code class="language-scala" >final implicit def AsFunction[F, Args <: scala.Tuple, R](f: <a href="./Expr.md">Expr</a>[F])(tf: TupledFunction[F, (Args) => R]): <a href="./Expr$/AsFunction.md">AsFunction</a>[F, Args, R]</pre></code>

### ExprOps
<pre><code class="language-scala" >final implicit def ExprOps[T](expr: <a href="./Expr.md">Expr</a>[T]): <a href="./Expr$/ExprOps.md">ExprOps</a>[T]</pre></code>

