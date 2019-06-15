example.level2.Documentation
# object IAmACaseClass

## Companion case class <a href="./IAmACaseClass.md">IAmACaseClass</a>

<pre><code class="language-scala" >final object IAmACaseClass extends (<a href="#T">T</a>, Int) => <a href="./IAmACaseClass.md">IAmACaseClass</a> with Serializable with Product</pre></code>
## Concrete Type Members:
### MirroredMonoType
<pre><code class="language-scala" >type MirroredMonoType: <a href="./IAmACaseClass.md">IAmACaseClass</a></pre></code>

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(x: <a href="#T">T</a>, id: Int): <a href="./IAmACaseClass.md">IAmACaseClass</a></pre></code>

### curried
<pre><code class="language-scala" >@unspecialized def curried: (T1) => (T2) => R</pre></code>

### fromProduct
<pre><code class="language-scala" >def fromProduct(x$0: Product): <a href="./IAmACaseClass$.md#MirroredMonoType">MirroredMonoType</a></pre></code>

### toString
<pre><code class="language-scala" >override def toString(): String</pre></code>

### tupled
<pre><code class="language-scala" >@unspecialized def tupled: ((T1, T2)) => R</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(x$1: <a href="./IAmACaseClass.md">IAmACaseClass</a>): <a href="./IAmACaseClass.md">IAmACaseClass</a></pre></code>

