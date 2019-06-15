scala.deriving$.Mirror$
# class SingletonProxy

<pre><code class="language-scala" >class SingletonProxy extends Product</pre></code>
A proxy for Scala 2 singletons, which do not inherit `Singleton` directly

## Constructors:
<pre><code class="language-scala" >SingletonProxy(value: AnyRef)</pre></code>

## Concrete Type Members:
### MirroredElemLabels
<pre><code class="language-scala" >type MirroredElemLabels: Unit</pre></code>

### MirroredElemTypes
<pre><code class="language-scala" >type MirroredElemTypes: Unit</pre></code>

### MirroredMonoType
<pre><code class="language-scala" >type MirroredMonoType: value</pre></code>

### MirroredType
<pre><code class="language-scala" >type MirroredType: value</pre></code>

## Concrete Value Members:
### fromProduct
<pre><code class="language-scala" >def fromProduct(p: Product): MirroredMonoType</pre></code>

### value
<pre><code class="language-scala" >val value: AnyRef</pre></code>

