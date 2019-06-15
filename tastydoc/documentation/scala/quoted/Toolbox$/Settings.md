scala.quoted.Toolbox$
# case class Settings

## Companion object <a href="./Settings$.md">Settings</a>

<pre><code class="language-scala" >case class Settings extends Product with Serializable</pre></code>
Setting of the Toolbox instance.

## Constructors:
<pre><code class="language-scala" >Settings(outDir: Option[String], showRawTree: Boolean, compilerArgs: List[String], color: Boolean)</pre></code>

## Concrete Value Members:
### _1
<pre><code class="language-scala" >def _1: Option[String]</pre></code>

### _2
<pre><code class="language-scala" >def _2: Boolean</pre></code>

### _3
<pre><code class="language-scala" >def _3: List[String]</pre></code>

### _4
<pre><code class="language-scala" >def _4: Boolean</pre></code>

### canEqual
<pre><code class="language-scala" >override def canEqual(that: Any): Boolean</pre></code>

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

### color
<pre><code class="language-scala" >val color: Boolean</pre></code>

### compilerArgs
<pre><code class="language-scala" >val compilerArgs: List[String]</pre></code>

### outDir
<pre><code class="language-scala" >val outDir: Option[String]</pre></code>

### showRawTree
<pre><code class="language-scala" >val showRawTree: Boolean</pre></code>

