scala
# object Eql

## Companion trait Eql

<pre><code class="language-scala" >final object Eql extends Serializable with Sum</pre></code>
Companion object containing a few universally known `Eql` instances.
Eql instances involving primitive types or the Null type are handled directly in
the compiler (see Implicits.synthesizedEq), so they are not included here.

## Concrete Type Members:
### derived
<pre><code class="language-scala" >final object derived</pre></code>
A universal `Eql` instance.

## Concrete Value Members:
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
<pre><code class="language-scala" >implicit def eqlSeq[T, U](implicit eq: Eql[T, U]): Eql[GenSeq[T], GenSeq[U]]</pre></code>

### eqlSet
<pre><code class="language-scala" >implicit def eqlSet[T, U](implicit eq: Eql[T, U]): Eql[Set[T], Set[U]]</pre></code>

### eqlString
<pre><code class="language-scala" >implicit def eqlString: Eql[String, String]</pre></code>

