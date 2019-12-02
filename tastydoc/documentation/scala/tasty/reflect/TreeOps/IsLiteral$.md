scala.tasty.reflect.TreeOps
# object IsLiteral

<pre><code class="language-scala" >final object IsLiteral extends Serializable</pre></code>
## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[Literal]</pre></code>
Matches any Literal and returns it

