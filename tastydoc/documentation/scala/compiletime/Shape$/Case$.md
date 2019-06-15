scala.compiletime.Shape$
# object Case

## Companion case class <a href="./Case.md">Case</a>

<pre><code class="language-scala" >final object Case extends Serializable with Product</pre></code>
A product type `T` with element types `Elems`

## Concrete Type Members:
### MirroredMonoType
<pre><code class="language-scala" >type MirroredMonoType: <a href="./Case.md">Case</a>[Nothing <: Any, Nothing <: Any]</pre></code>

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply[T, Elems](): <a href="./Case.md">Case</a>[T, Elems]</pre></code>

### fromProduct
<pre><code class="language-scala" >def fromProduct(x$0: Product): <a href="./Case$.md#MirroredMonoType">MirroredMonoType</a></pre></code>

### unapply
<pre><code class="language-scala" >def unapply[T, Elems](x$1: <a href="./Case.md">Case</a>[T, Elems]): Boolean</pre></code>

