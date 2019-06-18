scala.tasty.reflect.TreeOps
# object IsStatement

<pre><code class="language-scala" >final object IsStatement extends Serializable</pre></code>
## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[Statement]</pre></code>
Matches any Statement and returns it

