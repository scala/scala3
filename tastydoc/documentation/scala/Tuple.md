scala
# trait Tuple

## Companion object Tuple

<pre><code class="language-scala" >sealed trait Tuple extends Any</pre></code>
Tuple of arbitrary arity

## Known subclasses:
NonEmptyTuple
## Concrete Value Members:
### *:
<pre><code class="language-scala" >inline def *:[H, This >: this.type <: scala.Tuple](x: H): *:[H, This]</pre></code>
Return a new tuple by prepending the element to `this` tuple.
This opteration is O(this.size)

### ++
<pre><code class="language-scala" >inline def ++[This >: this.type <: scala.Tuple](that: Tuple): <a href="./Tuple.md#Concat">Concat</a>[This, that]</pre></code>
Return a new tuple by concatenating `this` tuple with `that` tuple.
This opteration is O(this.size + that.size)

### size
<pre><code class="language-scala" >inline def size[This >: this.type <: scala.Tuple]: <a href="./Tuple.md#Size">Size</a>[This]</pre></code>
Return the size (or arity) of the tuple

### toArray
<pre><code class="language-scala" >inline def toArray: Array[Object]</pre></code>
Create a copy this tuple as an Array

