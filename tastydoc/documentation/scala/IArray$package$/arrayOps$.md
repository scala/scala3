scala.IArray$package$
# object arrayOps

<pre><code class="language-scala" >final object arrayOps extends Serializable</pre></code>
Defines extension methods for immutable arrays

## Concrete Value Members:
### apply
<pre><code class="language-scala" >inline def apply[T](arr: IArray[T])(n: Int): T</pre></code>
The selection operation on an immutable array.

***return*** the element of the array at the given index

***n*** the index of the element to select

***arr*** the immutable array

### length
<pre><code class="language-scala" >inline def length[T](arr: IArray[T]): Int</pre></code>
The number of elements in an immutable array

***arr*** the immutable array

