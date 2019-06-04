scala
# trait NonEmptyTuple

<pre><code class="language-scala" >sealed trait NonEmptyTuple extends Tuple</pre></code>
## Constructors:
<pre><code class="language-scala" >NonEmptyTuple()</pre></code>

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### *:
<pre><code class="language-scala" >inline def *:[H, This >: this.type <: scala.Tuple](x: H): *:[H, This]</pre></code>

### ++
<pre><code class="language-scala" >inline def ++[This >: this.type <: scala.Tuple](that: Tuple): Concat[This, that]</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### apply
<pre><code class="language-scala" >inline def apply[This >: this.type <: scala.NonEmptyTuple](n: Int): Elem[This, n]</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): Boolean</pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: Any): Boolean</pre></code>

### fallbackApply
<pre><code class="language-scala" >inline def fallbackApply(n: Int): Elem[NonEmptyTuple, n]</pre></code>

### finalize
<pre><code class="language-scala" >protected def finalize(): Unit</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### head
<pre><code class="language-scala" >inline def head[This >: this.type <: scala.NonEmptyTuple]: Head[This]</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): Boolean</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

### size
<pre><code class="language-scala" >inline def size[This >: this.type <: scala.Tuple]: Size[This]</pre></code>

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### tail
<pre><code class="language-scala" >inline def tail[This >: this.type <: scala.NonEmptyTuple]: Tail[This]</pre></code>

### toArray
<pre><code class="language-scala" >inline def toArray: Array[Object]</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

