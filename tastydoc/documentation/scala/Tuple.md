scala
# trait Tuple

## Companion object Tuple

<pre><code class="language-scala" >sealed trait Tuple extends Any</pre></code>
Tuple of arbitrary arity

## Constructors:
<pre><code class="language-scala" >Tuple()</pre></code>

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### *:
<pre><code class="language-scala" >inline def *:[H, This >: this.type <: scala.Tuple](x: H): *:[H, This]</pre></code>
Return a new tuple by prepending the element to `this` tuple.
This opteration is O(this.size)

### ++
<pre><code class="language-scala" >inline def ++[This >: this.type <: scala.Tuple](that: Tuple): <a href="./Tuple.md#Concat">Concat</a>[This, that]</pre></code>
Return a new tuple by concatenating `this` tuple with `that` tuple.
This opteration is O(this.size + that.size)

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
<pre><code class="language-scala" >inline def size[This >: this.type <: scala.Tuple]: <a href="./Tuple.md#Size">Size</a>[This]</pre></code>
Return the size (or arity) of the tuple

### toArray
<pre><code class="language-scala" >inline def toArray: Array[Object]</pre></code>
Create a copy this tuple as an Array

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

