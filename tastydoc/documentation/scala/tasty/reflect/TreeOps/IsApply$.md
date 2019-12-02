scala.tasty.reflect.TreeOps
# object IsApply

<pre><code class="language-scala" >final object IsApply extends Serializable</pre></code>
## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[Apply]</pre></code>
Matches any Apply and returns it

