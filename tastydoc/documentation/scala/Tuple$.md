scala
# object Tuple

## Companion trait Tuple

<pre><code class="language-scala" >final object Tuple extends Serializable</pre></code>
## Concrete Type Members:
### Concat
<pre><code class="language-scala" >type Concat: [X >: scala.Nothing <: scala.Tuple, +Y >: scala.Nothing <: scala.Tuple] => X match  {
  scala.internal.MatchCase[scala.Unit, +Y]
  [x1 >: scala.Nothing <: scala.Any, xs1 >: scala.Nothing <: scala.Tuple] => scala.internal.MatchCase[scala.*:[x1, xs1], scala.*:[x1, scala.Tuple.Concat[xs1, +Y]]]
}</pre></code>
Type of the concatenation of two tuples


### Elem
<pre><code class="language-scala" >type Elem: [X >: scala.Nothing <: scala.Tuple, N >: scala.Nothing <: scala.Int] => X match  {
  [x >: scala.Nothing <: scala.Any, xs >: scala.Nothing <: scala.Tuple] => scala.internal.MatchCase[scala.*:[x, xs], N match  {
    scala.internal.MatchCase[0, x]
    [n1 >: scala.Nothing <: scala.Int] => scala.internal.MatchCase[scala.compiletime.S[n1], scala.Tuple.Elem[xs, n1]]
  }]
}</pre></code>
Type of the element a position N in the tuple X


### Head
<pre><code class="language-scala" >type Head: [X >: scala.Nothing <: scala.NonEmptyTuple] => X match  {
  [x >: scala.Nothing <: scala.Any, _ >: scala.Nothing <: scala.Tuple] => scala.internal.MatchCase[scala.*:[x, _], x]
}</pre></code>
Type of the head of a tuple


### Map
<pre><code class="language-scala" >type Map: [Tup >: scala.Nothing <: scala.Tuple, F >: scala.Nothing <: [_$1 >: scala.Nothing <: scala.Any] => scala.Any] => Tup match  {
  scala.internal.MatchCase[scala.Unit, scala.Unit]
  [h >: scala.Nothing <: scala.Any, t >: scala.Nothing <: scala.Tuple] => scala.internal.MatchCase[scala.*:[h, t], scala.*:[F[h], scala.Tuple.Map[t, F]]]
}</pre></code>
Converts a tuple `(T1, ..., Tn)` to `(F[T1], ..., F[Tn])`


### Size
<pre><code class="language-scala" >type Size: [X >: scala.Nothing <: scala.Tuple] => X match  {
  scala.internal.MatchCase[scala.Unit, 0]
  [x >: scala.Nothing <: scala.Any, xs >: scala.Nothing <: scala.Tuple] => scala.internal.MatchCase[scala.*:[x, xs], scala.compiletime.S[scala.Tuple.Size[xs]]]
}</pre></code>
Literal constant Int size of a tuple


### Tail
<pre><code class="language-scala" >type Tail: [X >: scala.Nothing <: scala.NonEmptyTuple] => X match  {
  [_ >: scala.Nothing <: scala.Any, xs >: scala.Nothing <: scala.Tuple] => scala.internal.MatchCase[scala.*:[_, xs], xs]
}</pre></code>
Type of the tail of a tuple


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
Convert an array into a tuple of unknown arity and types

### fromProduct
<pre><code class="language-scala" >def fromProduct(product: Product): Tuple</pre></code>
Convert a Product into a tuple of unknown arity and types

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

