scala.deriving$
# object Mirror

## Companion trait Mirror

<pre><code class="language-scala" >final object Mirror extends Serializable</pre></code>
## Known subclasses:
SingletonProxy, Singleton, Product, Sum
## Concrete Type Members:
### Of
<pre><code class="language-scala" >type Of: [T >: scala.Nothing <: scala.Any] => scala.deriving.Mirror {
  type MirroredType >: T <: T
  type MirroredMonoType >: T <: T
  type MirroredElemTypes >: scala.Nothing <: scala.Tuple
}</pre></code>

### ProductOf
<pre><code class="language-scala" >type ProductOf: [T >: scala.Nothing <: scala.Any] => scala.deriving.Mirror.Product {
  type MirroredType >: T <: T
  type MirroredMonoType >: T <: T
  type MirroredElemTypes >: scala.Nothing <: scala.Tuple
}</pre></code>

### SumOf
<pre><code class="language-scala" >type SumOf: [T >: scala.Nothing <: scala.Any] => scala.deriving.Mirror.Sum {
  type MirroredType >: T <: T
  type MirroredMonoType >: T <: T
  type MirroredElemTypes >: scala.Nothing <: scala.Tuple
}</pre></code>

### SingletonProxy
<pre><code class="language-scala" >class SingletonProxy</pre></code>
A proxy for Scala 2 singletons, which do not inherit `Singleton` directly

### Product
<pre><code class="language-scala" >trait Product</pre></code>
The Mirror for a product type

### Singleton
<pre><code class="language-scala" >trait Singleton</pre></code>
### Sum
<pre><code class="language-scala" >trait Sum</pre></code>
The Mirror for a sum type

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

