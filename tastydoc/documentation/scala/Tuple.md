scala
# trait Tuple

## Companion object Tuple

<pre><code class="language-scala" >sealed trait Tuple extends Any</pre></code>
## Constructors:
<pre><code class="language-scala" >Tuple()</pre></code>

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

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: Any): Boolean</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### size
<pre><code class="language-scala" >inline def size[This >: this.type <: scala.Tuple]: Size[This]</pre></code>

### toArray
<pre><code class="language-scala" >inline def toArray: Array[Object]</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

