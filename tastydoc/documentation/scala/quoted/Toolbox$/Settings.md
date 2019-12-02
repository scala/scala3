scala.quoted.Toolbox$
# case class Settings

## Companion object <a href="./Settings$.md">Settings</a>

<pre><code class="language-scala" >case class Settings extends Product with Serializable</pre></code>
Setting of the Toolbox instance.

## Constructors:
<pre><code class="language-scala" >Settings(outDir: Option[String], showRawTree: Boolean, compilerArgs: List[String], color: Boolean)</pre></code>

## Concrete Value Members:
### productIterator
<pre><code class="language-scala" >def productIterator: Iterator[Any]</pre></code>

### color
<pre><code class="language-scala" >val color: Boolean</pre></code>

### compilerArgs
<pre><code class="language-scala" >val compilerArgs: List[String]</pre></code>

### outDir
<pre><code class="language-scala" >val outDir: Option[String]</pre></code>

### showRawTree
<pre><code class="language-scala" >val showRawTree: Boolean</pre></code>

