scala.internal.quoted
# class LiftedExpr

<pre><code class="language-scala" >final class LiftedExpr[T] extends Expr[T]</pre></code>
An Expr backed by a lifted value.
Values can only be of type Boolean, Byte, Short, Char, Int, Long, Float, Double, Unit, String or Null.

## Constructors:
<pre><code class="language-scala" >LiftedExpr(value: T)</pre></code>

## Concrete Value Members:
### run
<pre><code class="language-scala" >final def run(toolbox: <a href="../../quoted/Toolbox.md">Toolbox</a>): <a href="../../quoted/Expr.md#T">T</a></pre></code>
Evaluate the contents of this expression and return the result.
May throw a FreeVariableError on expressions that came from a macro.

### toString
<pre><code class="language-scala" >override def toString: String</pre></code>

### value
<pre><code class="language-scala" >val value: T</pre></code>

