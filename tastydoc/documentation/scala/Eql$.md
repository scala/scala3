scala
# object Eql

## Companion trait Eql

<pre><code class="language-scala" >final object Eql extends Serializable with Sum</pre></code>
Companion object containing a few universally known `Eql` instances.
Eql instances involving primitive types or the Null type are handled directly in
the compiler (see Implicits.synthesizedEq), so they are not included here.

## Known subclasses:
derived
## Concrete Type Members:
### derived
<pre><code class="language-scala" >final object derived</pre></code>
A universal `Eql` instance.

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

### eqlAny
<pre><code class="language-scala" >def eqlAny[L, R]: Eql[L, R]</pre></code>
A fall-back instance to compare values of any types.
Even though this method is not declared a delegate, the compiler will
synthesize implicit arguments as solutions to `Eql[T, U]` queries if
the rules of multiversal equality require it.

### eqlNumber
<pre><code class="language-scala" >implicit def eqlNumber: Eql[Number, Number]</pre></code>

### eqlProxy
<pre><code class="language-scala" >implicit def eqlProxy: Eql[Proxy, AnyRef]</pre></code>

### eqlSeq
<pre><code class="language-scala" >implicit def eqlSeq[T, U](eq: Eql[T, U]): Eql[GenSeq[T], GenSeq[U]]</pre></code>

### eqlSet
<pre><code class="language-scala" >implicit def eqlSet[T, U](eq: Eql[T, U]): Eql[Set[T], Set[U]]</pre></code>

### eqlString
<pre><code class="language-scala" >implicit def eqlString: Eql[String, String]</pre></code>

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

