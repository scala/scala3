scala.internal
# class TypeBox

<pre><code class="language-scala" >final abstract class TypeBox[L <: U, U]</pre></code>
A type for skolems that are generated during capture conversion. Capture conversion
narrows the type of a tree whose type has wildcard arguments. A typical situation
is a tree `t` of type `C[_ >: L <: U]` and an expected type `C[X]` where `X` is an
instantiatable type variable. To be able to instantiate `X`, we cast the tree to type
`X[$n.CAP]` where `$n` is a fresh skolem type with underlying type `TypeBox[L, U]`.

## Constructors:
<pre><code class="language-scala" >TypeBox()</pre></code>

## Concrete Type Members:
### CAP
<pre><code class="language-scala" >type CAP: <a href="./TypeBox.md#L">L</a> <: <a href="./TypeBox.md#U">U</a></pre></code>

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

