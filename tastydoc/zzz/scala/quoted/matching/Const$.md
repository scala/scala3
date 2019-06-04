scala.quoted.matching
# object Const

<pre><code class="language-scala" >final object Const extends Serializable</pre></code>
Matches expressions containing literal constant values and extracts the value.
It may match expressions of type Boolean, Byte, Short, Int, Long,
Float, Double, Char, String, ClassTag, scala.Symbol, Null and Unit.
Usage:
```
(x: Expr[B]) match {
  case Const(value: B) => ...
}
```
``````

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### $asInstanceOf$
<pre><code class="language-scala" >final def $asInstanceOf$[X0]: X0</pre></code>

### $isInstanceOf$
<pre><code class="language-scala" >final def $isInstanceOf$[X0]: Boolean</pre></code>

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

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### unapply
<pre><code class="language-scala" >def unapply[T](expr: <a href="../Expr.md">Expr</a>[T])(reflect: <a href="../../tasty/Reflection.md">Reflection</a>): Option[T]</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

### writeReplace
<pre><code class="language-scala" >private def writeReplace(): AnyRef</pre></code>

