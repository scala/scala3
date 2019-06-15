scala.tasty.reflect
# trait ReportingOps

<pre><code class="language-scala" >trait ReportingOps extends Core</pre></code>
## Constructors:
<pre><code class="language-scala" >ReportingOps()</pre></code>

## Concrete Value Members:
### error
<pre><code class="language-scala" >def error(msg: => String, source: SourceFile, start: Int, end: Int)(ctx: Context): Unit</pre></code>

### error
<pre><code class="language-scala" >def error(msg: => String, pos: Position)(ctx: Context): Unit</pre></code>

### warning
<pre><code class="language-scala" >def warning(msg: => String, source: SourceFile, start: Int, end: Int)(ctx: Context): Unit</pre></code>

### warning
<pre><code class="language-scala" >def warning(msg: => String, pos: Position)(ctx: Context): Unit</pre></code>

