scala.tasty.reflect.TreeOps
# object Projection

<pre><code class="language-scala" >final object Projection extends Serializable</pre></code>
## Concrete Value Members:
### copy
<pre><code class="language-scala" >def copy(original: Projection)(qualifier: TypeTree, name: String)(ctx: Context): Projection</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(TypeTree, String)]</pre></code>

