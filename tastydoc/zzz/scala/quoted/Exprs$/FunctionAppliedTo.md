scala.quoted.Exprs$
# class FunctionAppliedTo

<pre><code class="language-scala" >final class FunctionAppliedTo[R] extends Expr[<a href="./FunctionAppliedTo.md#R">R</a>]</pre></code>
An Expr representing `'{($f).apply($x1, ..., $xn)}` but it is beta-reduced when the closure is known

## Constructors:
<pre><code class="language-scala" >FunctionAppliedTo(f: <a href="../Expr.md">Expr</a>[Nothing <: Any], args: Array[<a href="../Expr.md">Expr</a>[Nothing <: Any]])</pre></code>

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): Boolean</pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: Any): Boolean</pre></code>

### finalize
<pre><code class="language-scala" >protected def finalize(): Unit</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): Boolean</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

### run
<pre><code class="language-scala" >final def run(toolbox: <a href="../Toolbox.md">Toolbox</a>): <a href="../Expr.md#T">T</a></pre></code>
Evaluate the contents of this expression and return the result.
May throw a FreeVariableError on expressions that came from a macro.

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### toString
<pre><code class="language-scala" >override def toString: String</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

### args
<pre><code class="language-scala" >val args: Array[<a href="../Expr.md">Expr</a>[Nothing <: Any]]</pre></code>

### f
<pre><code class="language-scala" >val f: <a href="../Expr.md">Expr</a>[Nothing <: Any]</pre></code>

