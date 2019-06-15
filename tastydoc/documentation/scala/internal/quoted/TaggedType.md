scala.internal.quoted
# class TaggedType

<pre><code class="language-scala" >final class TaggedType[T] extends Type[T]</pre></code>
An Type backed by a value

## Constructors:
<pre><code class="language-scala" >TaggedType(ct: <a href="../../reflect/ClassTag.md">ClassTag</a>[T])</pre></code>

## Concrete Value Members:
### toString
<pre><code class="language-scala" >override def toString: String</pre></code>

### ct
<pre><code class="language-scala" >implicit val ct: <a href="../../reflect/ClassTag.md">ClassTag</a>[T]</pre></code>

