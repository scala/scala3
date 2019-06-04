scala
# object Tuple

## Companion trait Tuple

<pre><code class="language-scala" >final object Tuple extends Serializable</pre></code>
## Concrete Type Members:
### BoundedSize
<pre><code class="language-scala" >private[scala] type BoundedSize: [X >: scala.Nothing <: scala.Any] => scala.Tuple.BoundedSizeRecur[X, 23]</pre></code>

### BoundedSizeRecur
<pre><code class="language-scala" >private[scala] type BoundedSizeRecur: [X >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Int] => X match  {
  scala.internal.MatchCase[scala.Unit, 0]
  [x >: scala.Nothing <: scala.Any, xs >: scala.Nothing <: scala.Tuple] => scala.internal.MatchCase[scala.*:[x, xs], L match  {
    scala.internal.MatchCase[0, 0]
    [n >: scala.Nothing <: scala.Int] => scala.internal.MatchCase[scala.compiletime.S[n], scala.compiletime.S[scala.Tuple.BoundedSizeRecur[xs, n]]]
  }]
}</pre></code>

### Concat
<pre><code class="language-scala" >type Concat: [X >: scala.Nothing <: scala.Tuple, +Y >: scala.Nothing <: scala.Tuple] => X match  {
  scala.internal.MatchCase[scala.Unit, +Y]
  [x1 >: scala.Nothing <: scala.Any, xs1 >: scala.Nothing <: scala.Tuple] => scala.internal.MatchCase[scala.*:[x1, xs1], scala.*:[x1, scala.Tuple.Concat[xs1, +Y]]]
}</pre></code>

### Elem
<pre><code class="language-scala" >type Elem: [X >: scala.Nothing <: scala.Tuple, N >: scala.Nothing <: scala.Any] => X match  {
  [x >: scala.Nothing <: scala.Any, xs >: scala.Nothing <: scala.Tuple] => scala.internal.MatchCase[scala.*:[x, xs], N match  {
    scala.internal.MatchCase[0, x]
    [n1 >: scala.Nothing <: scala.Int] => scala.internal.MatchCase[scala.compiletime.S[n1], scala.Tuple.Elem[xs, n1]]
  }]
}</pre></code>

### Head
<pre><code class="language-scala" >type Head: [X >: scala.Nothing <: scala.NonEmptyTuple] => X match  {
  [x >: scala.Nothing <: scala.Any, _ >: scala.Nothing <: scala.Tuple] => scala.internal.MatchCase[scala.*:[x, _], x]
}</pre></code>

### Size
<pre><code class="language-scala" >type Size: [X >: scala.Nothing <: scala.Any] => X match  {
  scala.internal.MatchCase[scala.Unit, 0]
  [x >: scala.Nothing <: scala.Any, xs >: scala.Nothing <: scala.Tuple] => scala.internal.MatchCase[scala.*:[x, xs], scala.compiletime.S[scala.Tuple.Size[xs]]]
}</pre></code>

### Tail
<pre><code class="language-scala" >type Tail: [X >: scala.Nothing <: scala.NonEmptyTuple] => X match  {
  [_ >: scala.Nothing <: scala.Any, xs >: scala.Nothing <: scala.Tuple] => scala.internal.MatchCase[scala.*:[_, xs], xs]
}</pre></code>

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

### fromArray
<pre><code class="language-scala" >def fromArray[T](xs: Array[T]): Tuple</pre></code>

### fromProduct
<pre><code class="language-scala" >def fromProduct(product: Product): Tuple</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### knownTupleFromArray
<pre><code class="language-scala" >private[scala] inline def knownTupleFromArray[T <: scala.Tuple](xs: Array[Object]): T</pre></code>

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

### $emptyArray
<pre><code class="language-scala" >private[scala] val $emptyArray: Array[Object]</pre></code>

