scala
# object IArray$package

<pre><code class="language-scala" >final object IArray$package extends Serializable</pre></code>
## Concrete Type Members:
### IArray
<pre><code class="language-scala" >type IArray: [+T >: scala.Nothing <: scala.Any] => scala.Array[_ >: scala.Nothing <: +T]</pre></code>
An immutable array. An `IArray[T]` has the same representation as an `Array[T]`,
but it cannot be updated. Unlike regular arrays, immutable arrays are covariant.


### IArray
<pre><code class="language-scala" >final object IArray</pre></code>
### arrayOps
<pre><code class="language-scala" >final object arrayOps</pre></code>
Defines extension methods for immutable arrays

