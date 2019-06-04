dotty
# object DottyPredef

<pre><code class="language-scala" >final object DottyPredef extends Serializable</pre></code>
## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### $asInstanceOf$
<pre><code class="language-scala" >final def $asInstanceOf$[X0]: X0</pre></code>

### $isInstanceOf$
<pre><code class="language-scala" >final def $isInstanceOf$[X0]: Boolean</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### assert
<pre><code class="language-scala" >@forceInline final inline def assert(assertion: => Boolean): Unit</pre></code>

### assert
<pre><code class="language-scala" >@forceInline final inline def assert(assertion: => Boolean, message: => Any): Unit</pre></code>

### assertFail
<pre><code class="language-scala" >def assertFail(message: => Any): Unit</pre></code>

### assertFail
<pre><code class="language-scala" >def assertFail(): Unit</pre></code>

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

### implicitly
<pre><code class="language-scala" >@forceInline final inline def implicitly[T](ev: T): T</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### locally
<pre><code class="language-scala" >@forceInline inline def locally[T](body: => T): T</pre></code>

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): Boolean</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### the
<pre><code class="language-scala" >inline def the[T](x: T): x</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### valueOf
<pre><code class="language-scala" >inline def valueOf[T]: T</pre></code>
Retrieve the single value of a type with a unique inhabitant.

***Example*** 
```scala
object Foo
val foo = valueOf[Foo.type]
// foo is Foo.type = Foo
val bar = valueOf[23]
// bar is 23.type = 23
```

***Group*** utilities

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

### writeReplace
<pre><code class="language-scala" >private def writeReplace(): AnyRef</pre></code>

