scala.implicits
# object Not

## Companion class <a href="./Not.md">Not</a>

<pre><code class="language-scala" >final object Not extends <a href="./LowPriorityNot.md">LowPriorityNot</a> with Serializable</pre></code>
## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### amb1
<pre><code class="language-scala" >implicit def amb1[T](ev: T): <a href="./Not.md">Not</a>[T]</pre></code>
One of two ambiguous methods used to emulate negation in Scala 2

### amb2
<pre><code class="language-scala" >implicit def amb2[T](ev: T): <a href="./Not.md">Not</a>[T]</pre></code>
One of two ambiguous methods used to emulate negation in Scala 2

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### default
<pre><code class="language-scala" >implicit def default[T]: <a href="./Not.md">Not</a>[T]</pre></code>
A fallback method used to emulate negation in Scala 2

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

### value
<pre><code class="language-scala" >def value: <a href="./Not.md">Not</a>[Nothing]</pre></code>
A value of type `Not` to signal a successful search for `Not[C]` (i.e. a failing
search for `C`). A reference to this value will be explicitly constructed by Dotty's
implicit search algorithm

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

