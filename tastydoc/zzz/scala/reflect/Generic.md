scala.reflect
# class Generic

<pre><code class="language-scala" >abstract class Generic[T]</pre></code>
A class for mapping between an ADT value and
the case mirror that represents the value.

## Constructors:
<pre><code class="language-scala" >Generic()</pre></code>

## Concrete Type Members:
### Shape
<pre><code class="language-scala" >type Shape: Nothing <: <a href="../compiletime/Shape.md">Shape</a></pre></code>

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

### common
<pre><code class="language-scala" >def common: <a href="./GenericClass.md">GenericClass</a></pre></code>
The companion object of the ADT

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

### reflect
<pre><code class="language-scala" >def reflect(x: <a href="./Generic.md#T">T</a>): <a href="./Mirror.md">Mirror</a></pre></code>
The case mirror corresponding to ADT instance `x`

### reify
<pre><code class="language-scala" >def reify(mirror: <a href="./Mirror.md">Mirror</a>): <a href="./Generic.md#T">T</a></pre></code>
The ADT instance corresponding to given `mirror`

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

