scala.collection.immutable
# object List

## Companion class <a href="./List.md">List</a>

<pre><code class="language-scala" >final object List extends SeqFactory[[+A >: scala.Nothing <: scala.Any] => scala.collection.immutable.List[+A]] with <a href="../../Serializable.md">Serializable</a></pre></code>
$factoryInfo

## Known subclasses:
<a href="./List$/SerializationProxy.md">SerializationProxy</a>
## Concrete Type Members:
### SerializationProxy
<pre><code class="language-scala" >@<a href="../../SerialVersionUID.md">SerialVersionUID</a> private class <a href="./List$/SerializationProxy.md">SerializationProxy</a></pre></code>
## Concrete Value Members:
### ReusableCBF
<pre><code class="language-scala" >def ReusableCBF: GenericCanBuildFrom[<a href="../../Nothing.md">Nothing</a>]</pre></code>

### apply
<pre><code class="language-scala" >override def apply[A](xs: <a href="../Seq.md">Seq</a>[A]): List[A]</pre></code>

### canBuildFrom
<pre><code class="language-scala" >implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]]</pre></code>
$genericCanBuildFromInfo

### concat
<pre><code class="language-scala" >def concat[A](xss: <a href="../../<repeated>.md"><repeated></a>[Traversable[A]]): CC[A]</pre></code>

### empty
<pre><code class="language-scala" >override def empty[A]: List[A]</pre></code>

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

### iterate
<pre><code class="language-scala" >def iterate[A](start: A, len: <a href="../../Int.md">Int</a>)(f: (A) => A): CC[A]</pre></code>

### newBuilder
<pre><code class="language-scala" >def newBuilder[A]: Builder[A, List[A]]</pre></code>

### range
<pre><code class="language-scala" >def range[T](start: T, end: T, step: T)(evidence$2: Integral[T]): CC[T]</pre></code>

### range
<pre><code class="language-scala" >def range[T](start: T, end: T)(evidence$1: Integral[T]): CC[T]</pre></code>

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

### unapplySeq
<pre><code class="language-scala" >def unapplySeq[A](x: CC[A]): <a href="../../Some.md">Some</a>[CC[A]]</pre></code>

### partialNotApplied
<pre><code class="language-scala" >private[<a href="../../collection.md">collection</a>] val partialNotApplied: (<a href="../../Any.md">Any</a>) => <a href="../../Any.md">Any</a></pre></code>

