scala.tasty.reflect.TreeOps
# object IsApplied

<pre><code class="language-scala" >final object IsApplied extends Serializable</pre></code>
## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[Applied]</pre></code>
Matches any Applied and returns it

