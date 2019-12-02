scala.deriving$
# class ArrayProduct

<pre><code class="language-scala" >class ArrayProduct extends Product</pre></code>
Helper class to turn arrays into products

## Constructors:
<pre><code class="language-scala" >ArrayProduct(elems: Array[AnyRef])</pre></code>
<pre><code class="language-scala" >ArrayProduct(size: Int)</pre></code>

## Concrete Value Members:
### canEqual
<pre><code class="language-scala" >def canEqual(that: Any): Boolean</pre></code>

### productArity
<pre><code class="language-scala" >def productArity: Int</pre></code>

### productElement
<pre><code class="language-scala" >def productElement(n: Int): Any</pre></code>

### productIterator
<pre><code class="language-scala" >override def productIterator: Iterator[Any]</pre></code>

### productPrefix
<pre><code class="language-scala" >def productPrefix: String</pre></code>

### update
<pre><code class="language-scala" >def update(n: Int, x: Any): Unit</pre></code>

### elems
<pre><code class="language-scala" >val elems: Array[AnyRef]</pre></code>

