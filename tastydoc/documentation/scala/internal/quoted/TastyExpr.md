scala.internal.quoted
# class TastyExpr

<pre><code class="language-scala" >final class TastyExpr[T] extends Expr[T]</pre></code>
An Expr backed by a pickled TASTY tree

## Constructors:
<pre><code class="language-scala" >TastyExpr(tasty: <a href="../../runtime/quoted/Unpickler.md#Pickled">Pickled</a>, args: Seq[Any])</pre></code>

## Concrete Value Members:
### run
<pre><code class="language-scala" >final def run(implicit toolbox: <a href="../../quoted/Toolbox.md">Toolbox</a>): <a href="../../quoted/Expr.md#T">T</a></pre></code>
Evaluate the contents of this expression and return the result.
May throw a FreeVariableError on expressions that came from a macro.

### toString
<pre><code class="language-scala" >override def toString: String</pre></code>

### args
<pre><code class="language-scala" >val args: Seq[Any]</pre></code>

### tasty
<pre><code class="language-scala" >val tasty: Pickled</pre></code>

