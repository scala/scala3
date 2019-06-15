scala.compiletime.Shape$
# object Cases

## Companion case class <a href="./Cases.md">Cases</a>

<pre><code class="language-scala" >final object Cases extends Serializable with Product</pre></code>
A sum with alternative types `Alts`

## Concrete Type Members:
### MirroredMonoType
<pre><code class="language-scala" >type MirroredMonoType: <a href="./Cases.md">Cases</a>[Nothing <: Any]</pre></code>

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply[Alts](): <a href="./Cases.md">Cases</a>[Alts]</pre></code>

### fromProduct
<pre><code class="language-scala" >def fromProduct(x$0: Product): <a href="./Cases$.md#MirroredMonoType">MirroredMonoType</a></pre></code>

### unapply
<pre><code class="language-scala" >def unapply[Alts](x$1: <a href="./Cases.md">Cases</a>[Alts]): Boolean</pre></code>

