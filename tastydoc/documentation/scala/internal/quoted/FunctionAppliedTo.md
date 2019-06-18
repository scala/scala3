scala.internal.quoted
# class FunctionAppliedTo

<pre><code class="language-scala" >final class FunctionAppliedTo[R] extends Expr[R]</pre></code>
An Expr representing `'{($f).apply($x1, ..., $xn)}` but it is beta-reduced when the closure is known

## Constructors:
<pre><code class="language-scala" >FunctionAppliedTo(f: <a href="../../quoted/Expr.md">Expr</a>[Nothing <: Any], args: Array[<a href="../../quoted/Expr.md">Expr</a>[Nothing <: Any]])</pre></code>

## Concrete Value Members:
### run
<pre><code class="language-scala" >final def run(implicit toolbox: <a href="../../quoted/Toolbox.md">Toolbox</a>): <a href="../../quoted/Expr.md#T">T</a></pre></code>
Evaluate the contents of this expression and return the result.
May throw a FreeVariableError on expressions that came from a macro.

### toString
<pre><code class="language-scala" >override def toString: String</pre></code>

### args
<pre><code class="language-scala" >val args: Array[<a href="../../quoted/Expr.md">Expr</a>[Nothing <: Any]]</pre></code>

### f
<pre><code class="language-scala" >val f: <a href="../../quoted/Expr.md">Expr</a>[Nothing <: Any]</pre></code>

