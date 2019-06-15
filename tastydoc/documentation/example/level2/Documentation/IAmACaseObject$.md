example.level2.Documentation
# case object IAmACaseObject

<pre><code class="language-scala" >final case object IAmACaseObject extends <a href="./CaseImplementThis.md">CaseImplementThis</a> with Product with Serializable with Singleton</pre></code>
## Concrete Value Members:
### canEqual
<pre><code class="language-scala" >override def canEqual(that: Any): Boolean</pre></code>

### fromProduct
<pre><code class="language-scala" >def fromProduct(p: Product): MirroredMonoType</pre></code>

### hashCode
<pre><code class="language-scala" >override def hashCode(): Int</pre></code>

### productArity
<pre><code class="language-scala" >override def productArity: Int</pre></code>

### productElement
<pre><code class="language-scala" >override def productElement(n: Int): Any</pre></code>

### productIterator
<pre><code class="language-scala" >def productIterator: Iterator[Any]</pre></code>

### productPrefix
<pre><code class="language-scala" >override def productPrefix: String</pre></code>

### toString
<pre><code class="language-scala" >override def toString(): String</pre></code>

