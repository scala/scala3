scala.tasty.reflect.PatternOps.Pattern$
# object Bind

<pre><code class="language-scala" >final object Bind extends Serializable</pre></code>
## Concrete Value Members:
### copy
<pre><code class="language-scala" >def copy(original: Bind)(name: String, pattern: Pattern)(implicit ctx: Context): Bind</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(pattern: Pattern)(implicit ctx: Context): Option[(String, Pattern)]</pre></code>

