scala.reflect
# class Mirror

<pre><code class="language-scala" >class Mirror</pre></code>
A generic representation of a case in an ADT

***reflected*** The common class-specific part of this mirror

***elems*** The elements of the case

***ordinal*** The ordinal value of the case in the list of the ADT's cases

## Constructors:
<pre><code class="language-scala" >Mirror(adtClass: <a href="./GenericClass.md">GenericClass</a>, ordinal: Int, elems: Product)</pre></code>

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### apply
<pre><code class="language-scala" >def apply(n: Int): Any</pre></code>
The `n`'th element of this generic case

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### caseLabel
<pre><code class="language-scala" >def caseLabel: String</pre></code>
The name of the constructor of the case reflected by this mirror

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### elementLabel
<pre><code class="language-scala" >def elementLabel(n: Int): String</pre></code>
The label of the `n`'th element of the case reflected by this mirror

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

### adtClass
<pre><code class="language-scala" >val adtClass: <a href="./GenericClass.md">GenericClass</a></pre></code>

### elems
<pre><code class="language-scala" >val elems: Product</pre></code>

### ordinal
<pre><code class="language-scala" >val ordinal: Int</pre></code>

