dotty.runtime
# object LazyVals

<pre><code class="language-scala" >final object LazyVals extends Serializable</pre></code>
Helper methods used in thread-safe lazy vals.

## Known subclasses:
<a href="./LazyVals$/Names$.md">Names</a>
## Concrete Type Members:
### Names
<pre><code class="language-scala" >final object <a href="./LazyVals$/Names$.md">Names</a></pre></code>
## Concrete Value Members:
### CAS
<pre><code class="language-scala" >def CAS(t: Object, offset: Long, e: Long, v: Int, ord: Int): Boolean</pre></code>

### STATE
<pre><code class="language-scala" >def STATE(cur: Long, ord: Int): Long</pre></code>

### get
<pre><code class="language-scala" >def get(t: Object, off: Long): Long</pre></code>

### getOffset
<pre><code class="language-scala" >def getOffset(clz: Class[Nothing <: Any], name: String): Long</pre></code>

### setFlag
<pre><code class="language-scala" >def setFlag(t: Object, offset: Long, v: Int, ord: Int): Unit</pre></code>

### wait4Notification
<pre><code class="language-scala" >def wait4Notification(t: Object, offset: Long, cur: Long, ord: Int): Unit</pre></code>

### BITS_PER_LAZY_VAL
<pre><code class="language-scala" >final inline val BITS_PER_LAZY_VAL: 2</pre></code>

### Names
<pre><code class="language-scala" >final val Names: <a href="./LazyVals$/Names$.md">Names</a></pre></code>

