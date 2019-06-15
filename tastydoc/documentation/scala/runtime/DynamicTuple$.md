scala.runtime
# object DynamicTuple

<pre><code class="language-scala" >final object DynamicTuple extends Serializable</pre></code>
## Concrete Value Members:
### concatIterator
<pre><code class="language-scala" >def concatIterator(tup1: Tuple, tup2: Tuple): Iterator[Any]</pre></code>

### cons$Array
<pre><code class="language-scala" >def cons$Array[H](x: H, elems: Array[Object]): Array[Object]</pre></code>

### consIterator
<pre><code class="language-scala" >def consIterator(head: Any, tail: Tuple): Iterator[Any]</pre></code>

### dynamicApply
<pre><code class="language-scala" >def dynamicApply[This <: scala.NonEmptyTuple, N <: scala.Int](self: This, n: Int): Elem[This, N]</pre></code>

### dynamicConcat
<pre><code class="language-scala" >def dynamicConcat[This <: scala.Tuple, That <: scala.Tuple](self: This, that: That): Concat[This, That]</pre></code>

### dynamicCons
<pre><code class="language-scala" >def dynamicCons[H, This <: scala.Tuple](x: H, self: Tuple): *:[H, This]</pre></code>

### dynamicFromArray
<pre><code class="language-scala" >def dynamicFromArray[T <: scala.Tuple](xs: Array[Object]): T</pre></code>

### dynamicFromProduct
<pre><code class="language-scala" >def dynamicFromProduct[T <: scala.Tuple](xs: Product): T</pre></code>

### dynamicSize
<pre><code class="language-scala" >def dynamicSize[This <: scala.Tuple](self: This): Size[This]</pre></code>

### dynamicTail
<pre><code class="language-scala" >def dynamicTail[This <: scala.NonEmptyTuple](self: This): Tail[This]</pre></code>

### dynamicToArray
<pre><code class="language-scala" >def dynamicToArray(self: Tuple): Array[Object]</pre></code>

### productToArray
<pre><code class="language-scala" >def productToArray(self: Product): Array[Object]</pre></code>

### to$Array
<pre><code class="language-scala" >def to$Array(xs: Tuple, n: Int): Array[Object]</pre></code>

### MaxSpecialized
<pre><code class="language-scala" >inline val MaxSpecialized: 22</pre></code>

