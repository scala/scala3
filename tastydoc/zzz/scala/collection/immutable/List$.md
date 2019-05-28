# object List

## Companion class <a href="./List.md">List</a>

<pre><code class="language-scala" >final object List extends SeqFactory[[+A >: scala.Nothing <: scala.Any] => scala.collection.immutable.List[+A]] with <a href="../../Serializable.md">Serializable</a></pre></code>
$factoryInfo
## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: <a href="../../Any.md">Any</a>): <a href="../../Boolean.md">Boolean</a></pre></code>

### ##
<pre><code class="language-scala" >final def ##: <a href="../../Int.md">Int</a></pre></code>

### $asInstanceOf$
<pre><code class="language-scala" >final def $asInstanceOf$[X0]: X0</pre></code>

### $isInstanceOf$
<pre><code class="language-scala" >final def $isInstanceOf$[X0]: <a href="../../Boolean.md">Boolean</a></pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: <a href="../../Any.md">Any</a>): <a href="../../Boolean.md">Boolean</a></pre></code>

### ReusableCBF
<pre><code class="language-scala" >def ReusableCBF: GenericCanBuildFrom[<a href="../../Nothing.md">Nothing</a>]</pre></code>

### apply
<pre><code class="language-scala" >override def apply[A](xs: <a href="../Seq.md">Seq</a>[A]): List[A]</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### canBuildFrom
<pre><code class="language-scala" >implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]]</pre></code>
$genericCanBuildFromInfo
### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### concat
<pre><code class="language-scala" >def concat[A](xss: <a href="../../<repeated>.md"><repeated></a>[Traversable[A]]): CC[A]</pre></code>

### empty
<pre><code class="language-scala" >override def empty[A]: List[A]</pre></code>

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): <a href="../../Boolean.md">Boolean</a></pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: <a href="../../Any.md">Any</a>): <a href="../../Boolean.md">Boolean</a></pre></code>

### fill
<pre><code class="language-scala" >def fill[A](n1: <a href="../../Int.md">Int</a>, n2: <a href="../../Int.md">Int</a>, n3: <a href="../../Int.md">Int</a>, n4: <a href="../../Int.md">Int</a>, n5: <a href="../../Int.md">Int</a>)(elem: => A): CC[CC[CC[CC[CC[A]]]]]</pre></code>

### fill
<pre><code class="language-scala" >def fill[A](n1: <a href="../../Int.md">Int</a>, n2: <a href="../../Int.md">Int</a>, n3: <a href="../../Int.md">Int</a>, n4: <a href="../../Int.md">Int</a>)(elem: => A): CC[CC[CC[CC[A]]]]</pre></code>

### fill
<pre><code class="language-scala" >def fill[A](n1: <a href="../../Int.md">Int</a>, n2: <a href="../../Int.md">Int</a>, n3: <a href="../../Int.md">Int</a>)(elem: => A): CC[CC[CC[A]]]</pre></code>

### fill
<pre><code class="language-scala" >def fill[A](n1: <a href="../../Int.md">Int</a>, n2: <a href="../../Int.md">Int</a>)(elem: => A): CC[CC[A]]</pre></code>

### fill
<pre><code class="language-scala" >def fill[A](n: <a href="../../Int.md">Int</a>)(elem: => A): CC[A]</pre></code>

### finalize
<pre><code class="language-scala" >protected def finalize(): <a href="../../Unit.md">Unit</a></pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): <a href="../../Int.md">Int</a></pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: <a href="../../Boolean.md">Boolean</a></pre></code>

### iterate
<pre><code class="language-scala" >def iterate[A](start: A, len: <a href="../../Int.md">Int</a>)(f: (A) => A): CC[A]</pre></code>

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): <a href="../../Boolean.md">Boolean</a></pre></code>

### newBuilder
<pre><code class="language-scala" >def newBuilder[A]: Builder[A, List[A]]</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): <a href="../../Unit.md">Unit</a></pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): <a href="../../Unit.md">Unit</a></pre></code>

### range
<pre><code class="language-scala" >def range[T](start: T, end: T, step: T)(evidence$2: Integral[T]): CC[T]</pre></code>

### range
<pre><code class="language-scala" >def range[T](start: T, end: T)(evidence$1: Integral[T]): CC[T]</pre></code>

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### tabulate
<pre><code class="language-scala" >def tabulate[A](n1: <a href="../../Int.md">Int</a>, n2: <a href="../../Int.md">Int</a>, n3: <a href="../../Int.md">Int</a>, n4: <a href="../../Int.md">Int</a>, n5: <a href="../../Int.md">Int</a>)(f: (<a href="../../Int.md">Int</a>, <a href="../../Int.md">Int</a>, <a href="../../Int.md">Int</a>, <a href="../../Int.md">Int</a>, <a href="../../Int.md">Int</a>) => A): CC[CC[CC[CC[CC[A]]]]]</pre></code>

### tabulate
<pre><code class="language-scala" >def tabulate[A](n1: <a href="../../Int.md">Int</a>, n2: <a href="../../Int.md">Int</a>, n3: <a href="../../Int.md">Int</a>, n4: <a href="../../Int.md">Int</a>)(f: (<a href="../../Int.md">Int</a>, <a href="../../Int.md">Int</a>, <a href="../../Int.md">Int</a>, <a href="../../Int.md">Int</a>) => A): CC[CC[CC[CC[A]]]]</pre></code>

### tabulate
<pre><code class="language-scala" >def tabulate[A](n1: <a href="../../Int.md">Int</a>, n2: <a href="../../Int.md">Int</a>, n3: <a href="../../Int.md">Int</a>)(f: (<a href="../../Int.md">Int</a>, <a href="../../Int.md">Int</a>, <a href="../../Int.md">Int</a>) => A): CC[CC[CC[A]]]</pre></code>

### tabulate
<pre><code class="language-scala" >def tabulate[A](n1: <a href="../../Int.md">Int</a>, n2: <a href="../../Int.md">Int</a>)(f: (<a href="../../Int.md">Int</a>, <a href="../../Int.md">Int</a>) => A): CC[CC[A]]</pre></code>

### tabulate
<pre><code class="language-scala" >def tabulate[A](n: <a href="../../Int.md">Int</a>)(f: (<a href="../../Int.md">Int</a>) => A): CC[A]</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### unapplySeq
<pre><code class="language-scala" >def unapplySeq[A](x: CC[A]): <a href="../../Some.md">Some</a>[CC[A]]</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: <a href="../../Long.md">Long</a>, x$1: <a href="../../Int.md">Int</a>): <a href="../../Unit.md">Unit</a></pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: <a href="../../Long.md">Long</a>): <a href="../../Unit.md">Unit</a></pre></code>

### wait
<pre><code class="language-scala" >final def wait(): <a href="../../Unit.md">Unit</a></pre></code>

### writeReplace
<pre><code class="language-scala" >private def writeReplace(): <a href="../../AnyRef.md">AnyRef</a></pre></code>

### partialNotApplied
<pre><code class="language-scala" >private[<a href="../../collection.md">collection</a>] val partialNotApplied: (<a href="../../Any.md">Any</a>) => <a href="../../Any.md">Any</a></pre></code>

