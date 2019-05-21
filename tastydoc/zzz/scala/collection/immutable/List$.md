# object List

## Companion class List

<pre><code class="language-scala" >final object List extends SeqFactory[[+A >: scala.Nothing <: scala.Any] => scala.collection.immutable.List[+A]] with Serializable</pre></code>
$factoryInfo

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

### ReusableCBF
<pre><code class="language-scala" >def ReusableCBF: GenericCanBuildFrom[Nothing]</pre></code>

### apply
<pre><code class="language-scala" >override def apply[A](xs: Seq[A]): List[A]</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### canBuildFrom
<pre><code class="language-scala" >implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]]</pre></code>
$genericCanBuildFromInfo

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### concat
<pre><code class="language-scala" >def concat[A](xss: <repeated>[Traversable[A]]): CC[A]</pre></code>

### empty
<pre><code class="language-scala" >override def empty[A]: List[A]</pre></code>

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): Boolean</pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: Any): Boolean</pre></code>

### fill
<pre><code class="language-scala" >def fill[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => A): CC[CC[CC[CC[CC[A]]]]]</pre></code>

### fill
<pre><code class="language-scala" >def fill[A](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): CC[CC[CC[CC[A]]]]</pre></code>

### fill
<pre><code class="language-scala" >def fill[A](n1: Int, n2: Int, n3: Int)(elem: => A): CC[CC[CC[A]]]</pre></code>

### fill
<pre><code class="language-scala" >def fill[A](n1: Int, n2: Int)(elem: => A): CC[CC[A]]</pre></code>

### fill
<pre><code class="language-scala" >def fill[A](n: Int)(elem: => A): CC[A]</pre></code>

### finalize
<pre><code class="language-scala" >protected def finalize(): Unit</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### iterate
<pre><code class="language-scala" >def iterate[A](start: A, len: Int)(f: (A) => A): CC[A]</pre></code>

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): Boolean</pre></code>

### newBuilder
<pre><code class="language-scala" >def newBuilder[A]: Builder[A, List[A]]</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

### range
<pre><code class="language-scala" >def range[T](start: T, end: T, step: T)(evidence$2: Integral[T]): CC[T]</pre></code>

### range
<pre><code class="language-scala" >def range[T](start: T, end: T)(evidence$1: Integral[T]): CC[T]</pre></code>

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### tabulate
<pre><code class="language-scala" >def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A): CC[CC[CC[CC[CC[A]]]]]</pre></code>

### tabulate
<pre><code class="language-scala" >def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => A): CC[CC[CC[CC[A]]]]</pre></code>

### tabulate
<pre><code class="language-scala" >def tabulate[A](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): CC[CC[CC[A]]]</pre></code>

### tabulate
<pre><code class="language-scala" >def tabulate[A](n1: Int, n2: Int)(f: (Int, Int) => A): CC[CC[A]]</pre></code>

### tabulate
<pre><code class="language-scala" >def tabulate[A](n: Int)(f: (Int) => A): CC[A]</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### unapplySeq
<pre><code class="language-scala" >def unapplySeq[A](x: CC[A]): Some[CC[A]]</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

### writeReplace
<pre><code class="language-scala" >private def writeReplace(): AnyRef</pre></code>

### partialNotApplied
<pre><code class="language-scala" >private[collection] val partialNotApplied: (Any) => Any</pre></code>

