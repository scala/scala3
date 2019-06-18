scala.tasty.reflect.TreeOps
# object IsTypeApply

<pre><code class="language-scala" >final object IsTypeApply extends Serializable</pre></code>
## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[TypeApply]</pre></code>
Matches any TypeApply and returns it

