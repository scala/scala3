scala.quoted
# object Liftable

## Companion class <a href="./Liftable.md">Liftable</a>

<pre><code class="language-scala" >final object Liftable extends Serializable</pre></code>
Some liftable base types. To be completed with at least all types
that are valid Scala literals. The actual implementation of these
typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
gives an alternative implementation using just the basic staging system.

## Known subclasses:
<a href="./Liftable$/PrimitiveLiftable.md">PrimitiveLiftable</a>
## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### ClassIsLiftable
<pre><code class="language-scala" >implicit def ClassIsLiftable[T]: <a href="./Liftable.md">Liftable</a>[Class[T]]</pre></code>

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

### Liftable_Boolean_delegate
<pre><code class="language-scala" >implicit val Liftable_Boolean_delegate: <a href="./Liftable.md">Liftable</a>[Boolean]</pre></code>

### Liftable_Char_delegate
<pre><code class="language-scala" >implicit val Liftable_Char_delegate: <a href="./Liftable.md">Liftable</a>[Char]</pre></code>

### Liftable_Double_delegate
<pre><code class="language-scala" >implicit val Liftable_Double_delegate: <a href="./Liftable.md">Liftable</a>[Double]</pre></code>

### Liftable_Float_delegate
<pre><code class="language-scala" >implicit val Liftable_Float_delegate: <a href="./Liftable.md">Liftable</a>[Float]</pre></code>

### Liftable_Int_delegate
<pre><code class="language-scala" >implicit val Liftable_Int_delegate: <a href="./Liftable.md">Liftable</a>[Int]</pre></code>

### Liftable_Long_delegate
<pre><code class="language-scala" >implicit val Liftable_Long_delegate: <a href="./Liftable.md">Liftable</a>[Long]</pre></code>

### Liftable_Short_delegate
<pre><code class="language-scala" >implicit val Liftable_Short_delegate: <a href="./Liftable.md">Liftable</a>[Short]</pre></code>

### Liftable_String_delegate
<pre><code class="language-scala" >implicit val Liftable_String_delegate: <a href="./Liftable.md">Liftable</a>[String]</pre></code>

