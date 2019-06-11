scalaShadowing.language$
# object experimental

<pre><code class="language-scala" >final object experimental extends Serializable</pre></code>
The experimental object contains features that have been recently added but have not
been thoroughly tested in production yet.
Experimental features **may undergo API changes** in future releases, so production
code should not rely on them.
Programmers are encouraged to try out experimental features and
[report any bugs or API inconsistencies](http://issues.scala-lang.org)
they encounter so they can be improved in future releases.

***Group*** experimental

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

### macros
<pre><code class="language-scala" >@volatile implicit val macros: macros</pre></code>
Where enabled, macro definitions are allowed. Macro implementations and
macro applications are unaffected; they can be used anywhere.
**Why introduce the feature?** Macros promise to make the language more regular,
replacing ad-hoc language constructs with a general powerful abstraction
capability that can express them. Macros are also a more disciplined and
powerful replacement for compiler plugins.
**Why control it?** For their very power, macros can lead to code that is hard
to debug and understand.


