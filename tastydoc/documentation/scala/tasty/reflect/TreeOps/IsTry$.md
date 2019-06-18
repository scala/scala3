scala.tasty.reflect.TreeOps
# object IsTry

<pre><code class="language-scala" >final object IsTry extends Serializable</pre></code>
## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[Try]</pre></code>
Matches any Try and returns it

