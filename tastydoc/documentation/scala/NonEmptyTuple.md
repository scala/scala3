scala
# trait NonEmptyTuple

<pre><code class="language-scala" >sealed trait NonEmptyTuple extends Tuple</pre></code>
Tuple of arbitrary non-zero arity

## Known subclasses:
*:
## Constructors:
<pre><code class="language-scala" >NonEmptyTuple()</pre></code>

## Concrete Value Members:
### *:
<pre><code class="language-scala" >inline def *:[H, This >: this.type <: scala.Tuple](x: H): *:[H, This]</pre></code>
Return a new tuple by prepending the element to `this` tuple.
This opteration is O(this.size)

### ++
<pre><code class="language-scala" >inline def ++[This >: this.type <: scala.Tuple](that: Tuple): <a href="./Tuple.md#Concat">Concat</a>[This, that]</pre></code>
Return a new tuple by concatenating `this` tuple with `that` tuple.
This opteration is O(this.size + that.size)

### apply
<pre><code class="language-scala" >inline def apply[This >: this.type <: scala.NonEmptyTuple](n: Int): Elem[This, n]</pre></code>
Get the i-th element of this tuple.
Equivalent to productElement but with a precise return type.

### head
<pre><code class="language-scala" >inline def head[This >: this.type <: scala.NonEmptyTuple]: Head[This]</pre></code>
Get the head of this tuple

### size
<pre><code class="language-scala" >inline def size[This >: this.type <: scala.Tuple]: <a href="./Tuple.md#Size">Size</a>[This]</pre></code>
Return the size (or arity) of the tuple

### tail
<pre><code class="language-scala" >inline def tail[This >: this.type <: scala.NonEmptyTuple]: Tail[This]</pre></code>
Get the tail of this tuple.
This opteration is O(this.size)

### toArray
<pre><code class="language-scala" >inline def toArray: Array[Object]</pre></code>
Create a copy this tuple as an Array

