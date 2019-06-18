scala.tasty.reflect.TreeOps
# object IsIdent

<pre><code class="language-scala" >final object IsIdent extends Serializable</pre></code>
## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[Ident]</pre></code>
Matches any Ident and returns it

