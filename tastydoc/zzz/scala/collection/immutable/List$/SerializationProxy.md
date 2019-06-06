scala.collection.immutable.List$
# class SerializationProxy

<pre><code class="language-scala" >private class SerializationProxy[A] extends <a href="../../../Serializable.md">Serializable</a></pre></code>
## Annotations:
@<a href="../../../SerialVersionUID.md">SerialVersionUID</a> 
## Constructors:
<pre><code class="language-scala" >SerializationProxy(orig: List[A])</pre></code>

## Concrete Value Members:
### orig_=
<pre><code class="language-scala" >@<a href="../../../transient.md">transient</a> private def orig_=(x$1: List[A]): <a href="../../../Unit.md">Unit</a></pre></code>

### readObject
<pre><code class="language-scala" >private def readObject(in: ObjectInputStream): <a href="../../../Unit.md">Unit</a></pre></code>

### readResolve
<pre><code class="language-scala" >private def readResolve(): <a href="../../../AnyRef.md">AnyRef</a></pre></code>

### writeObject
<pre><code class="language-scala" >private def writeObject(out: ObjectOutputStream): <a href="../../../Unit.md">Unit</a></pre></code>

### orig
<pre><code class="language-scala" >@<a href="../../../transient.md">transient</a> private var orig: List[A]</pre></code>

