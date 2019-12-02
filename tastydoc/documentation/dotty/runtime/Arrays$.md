dotty.runtime
# object Arrays

<pre><code class="language-scala" >final object Arrays extends Serializable</pre></code>
All but the first two operations should be short-circuited and implemented specially by
the backend.

## Concrete Value Members:
### newArray
<pre><code class="language-scala" >def newArray[Arr](componentType: Class[Nothing <: Any], returnType: Class[Arr], dimensions: Array[Int]): Arr</pre></code>
Create an array of a reference type T.

### newGenericArray
<pre><code class="language-scala" >def newGenericArray[T](length: Int)(implicit tag: <a href="../../scala/reflect/ClassTag.md">ClassTag</a>[T]): Array[T]</pre></code>
Creates an array of some element type determined by the given `ClassTag`
argument. The erased type of applications of this method is `Object`.

### seqToArray
<pre><code class="language-scala" >def seqToArray[T](xs: Seq[T], clazz: Class[Nothing <: Any]): Array[T]</pre></code>
Convert a sequence to a Java array with element type given by `clazz`.

