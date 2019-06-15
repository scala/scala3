scala.deriving$
# object Mirror

## Companion trait Mirror

<pre><code class="language-scala" >final object Mirror extends Serializable</pre></code>
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

