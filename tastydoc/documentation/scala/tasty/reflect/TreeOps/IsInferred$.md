scala.tasty.reflect.TreeOps
# object IsInferred

<pre><code class="language-scala" >final object IsInferred extends Serializable</pre></code>
## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[Inferred]</pre></code>
Matches any Inferred and returns it

