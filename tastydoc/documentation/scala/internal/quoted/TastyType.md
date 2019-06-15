scala.internal.quoted
# class TastyType

<pre><code class="language-scala" >final class TastyType[T] extends Type[T]</pre></code>
A Type backed by a pickled TASTY tree

## Constructors:
<pre><code class="language-scala" >TastyType(tasty: Pickled, args: Seq[Any])</pre></code>

## Concrete Value Members:
### toString
<pre><code class="language-scala" >override def toString(): String</pre></code>

### args
<pre><code class="language-scala" >val args: Seq[Any]</pre></code>

### tasty
<pre><code class="language-scala" >val tasty: Pickled</pre></code>

