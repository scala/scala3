dotty.runtime
# object LazyVals

<pre><code class="language-scala" >final object LazyVals extends Serializable</pre></code>
Helper methods used in thread-safe lazy vals.

## Known subclasses:
<a href="./LazyVals$/Names$.md">Names</a>
## Concrete Type Members:
### Names$
<pre><code class="language-scala" >final object <a href="./LazyVals$/Names$.md">Names</a></pre></code>
## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### CAS
<pre><code class="language-scala" >def CAS(t: Object, offset: Long, e: Long, v: Int, ord: Int): Boolean</pre></code>

### STATE
<pre><code class="language-scala" >def STATE(cur: Long, ord: Int): Long</pre></code>

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

### get
<pre><code class="language-scala" >def get(t: Object, off: Long): Long</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### getOffset
<pre><code class="language-scala" >def getOffset(clz: Class[Nothing <: Any], name: String): Long</pre></code>

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

### setFlag
<pre><code class="language-scala" >def setFlag(t: Object, offset: Long, v: Int, ord: Int): Unit</pre></code>

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

### wait4Notification
<pre><code class="language-scala" >def wait4Notification(t: Object, offset: Long, cur: Long, ord: Int): Unit</pre></code>

### BITS_PER_LAZY_VAL
<pre><code class="language-scala" >final inline val BITS_PER_LAZY_VAL: 2</pre></code>

### Names
<pre><code class="language-scala" >final val Names: <a href="./LazyVals$/Names$.md">Names</a></pre></code>

