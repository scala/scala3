scala.compiletime.Shape$
# case class Cases

## Companion object <a href="./Cases$.md">Cases</a>

<pre><code class="language-scala" >case class Cases[Alts <: scala.Tuple] extends Shape with Product with Serializable</pre></code>
A sum with alternative types `Alts`

## Constructors:
<pre><code class="language-scala" >Cases()</pre></code>

## Concrete Value Members:
### canEqual
<pre><code class="language-scala" >override def canEqual(that: Any): Boolean</pre></code>

### copy
<pre><code class="language-scala" >def copy[Alts](): <a href="./Cases.md">Cases</a>[Alts]</pre></code>

### equals
<pre><code class="language-scala" >override def equals(x$0: Any): Boolean</pre></code>

### hashCode
<pre><code class="language-scala" >override def hashCode(): Int</pre></code>

### productArity
<pre><code class="language-scala" >override def productArity: Int</pre></code>

### productElement
<pre><code class="language-scala" >override def productElement(n: Int): Any</pre></code>

### productElementName
<pre><code class="language-scala" >def productElementName(x$1: Int): String</pre></code>

### productIterator
<pre><code class="language-scala" >def productIterator: Iterator[Any]</pre></code>

### productPrefix
<pre><code class="language-scala" >override def productPrefix: String</pre></code>

### toString
<pre><code class="language-scala" >override def toString(): String</pre></code>

