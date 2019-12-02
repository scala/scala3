scala.tasty.reflect
# trait ReportingOps

<pre><code class="language-scala" >trait ReportingOps extends Core</pre></code>
## Concrete Value Members:
### error
<pre><code class="language-scala" >def error(msg: => String, source: SourceFile, start: Int, end: Int)(implicit ctx: Context): Unit</pre></code>

### error
<pre><code class="language-scala" >def error(msg: => String, pos: Position)(implicit ctx: Context): Unit</pre></code>

### warning
<pre><code class="language-scala" >def warning(msg: => String, source: SourceFile, start: Int, end: Int)(implicit ctx: Context): Unit</pre></code>

### warning
<pre><code class="language-scala" >def warning(msg: => String, pos: Position)(implicit ctx: Context): Unit</pre></code>

