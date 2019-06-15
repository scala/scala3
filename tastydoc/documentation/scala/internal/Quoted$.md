scala.internal
# object Quoted

<pre><code class="language-scala" >final object Quoted extends Serializable</pre></code>
## Known subclasses:
<a href="./Quoted$/patternBindHole.md">patternBindHole</a>
## Concrete Type Members:
### patternBindHole
<pre><code class="language-scala" >@compileTimeOnly class <a href="./Quoted$/patternBindHole.md">patternBindHole</a></pre></code>
A splice of a name in a quoted pattern is desugared by wrapping getting this annotation

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

### exprQuote
<pre><code class="language-scala" >@compileTimeOnly def exprQuote[T](x: T): <a href="../quoted/Expr.md">Expr</a>[T]</pre></code>
A term quote is desugared by the compiler into a call to this method

### exprSplice
<pre><code class="language-scala" >@compileTimeOnly def exprSplice[T](x: <a href="../quoted/Expr.md">Expr</a>[T]): T</pre></code>
A term splice is desugared by the compiler into a call to this method

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

### patternHole
<pre><code class="language-scala" >@compileTimeOnly def patternHole[T]: T</pre></code>
A splice in a quoted pattern is desugared by the compiler into a call to this method

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### typeQuote
<pre><code class="language-scala" >@compileTimeOnly def typeQuote[T <: scala.AnyKind]: <a href="../quoted/Type.md">Type</a>[T]</pre></code>
A type quote is desugared by the compiler into a call to this method

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

