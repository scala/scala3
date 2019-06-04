dotty.runtime
# object Arrays

<pre><code class="language-scala" >final object Arrays extends Serializable</pre></code>
All but the first two operations should be short-circuited and implemented specially by
the backend.

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

### newArray
<pre><code class="language-scala" >def newArray[Arr](componentType: Class[Nothing <: Any], returnType: Class[Arr], dimensions: Array[Int]): Arr</pre></code>
Create an array of a reference type T.

### newGenericArray
<pre><code class="language-scala" >def newGenericArray[T](length: Int)(tag: <a href="../../scala/reflect/ClassTag.md">ClassTag</a>[T]): Array[T]</pre></code>
Creates an array of some element type determined by the given `ClassTag`
argument. The erased type of applications of this method is `Object`.

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

### seqToArray
<pre><code class="language-scala" >def seqToArray[T](xs: Seq[T], clazz: Class[Nothing <: Any]): Array[T]</pre></code>
Convert a sequence to a Java array with element type given by `clazz`.

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

### writeReplace
<pre><code class="language-scala" >private def writeReplace(): AnyRef</pre></code>

