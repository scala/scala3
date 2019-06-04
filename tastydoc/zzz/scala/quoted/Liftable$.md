scala.quoted
# object Liftable

## Companion class <a href="./Liftable.md">Liftable</a>

<pre><code class="language-scala" >final object Liftable extends Serializable</pre></code>
Some liftable base types. To be completed with at least all types
that are valid Scala literals. The actual implementation of these
typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
gives an alternative implementation using just the basic staging system.

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### ClassIsLiftable
<pre><code class="language-scala" >final def ClassIsLiftable[T]: <a href="./Liftable.md">Liftable</a>[Class[T]]</pre></code>

### Liftable_Boolean_instance
<pre><code class="language-scala" >final def Liftable_Boolean_instance: <a href="./Liftable.md">Liftable</a>[Boolean]</pre></code>

### Liftable_Char_instance
<pre><code class="language-scala" >final def Liftable_Char_instance: <a href="./Liftable.md">Liftable</a>[Char]</pre></code>

### Liftable_Double_instance
<pre><code class="language-scala" >final def Liftable_Double_instance: <a href="./Liftable.md">Liftable</a>[Double]</pre></code>

### Liftable_Float_instance
<pre><code class="language-scala" >final def Liftable_Float_instance: <a href="./Liftable.md">Liftable</a>[Float]</pre></code>

### Liftable_Int_instance
<pre><code class="language-scala" >final def Liftable_Int_instance: <a href="./Liftable.md">Liftable</a>[Int]</pre></code>

### Liftable_Long_instance
<pre><code class="language-scala" >final def Liftable_Long_instance: <a href="./Liftable.md">Liftable</a>[Long]</pre></code>

### Liftable_Short_instance
<pre><code class="language-scala" >final def Liftable_Short_instance: <a href="./Liftable.md">Liftable</a>[Short]</pre></code>

### Liftable_String_instance
<pre><code class="language-scala" >final def Liftable_String_instance: <a href="./Liftable.md">Liftable</a>[String]</pre></code>

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

