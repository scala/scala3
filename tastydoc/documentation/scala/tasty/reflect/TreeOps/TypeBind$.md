scala.tasty.reflect.TreeOps
# object TypeBind

<pre><code class="language-scala" >final object TypeBind extends Serializable</pre></code>
## Concrete Value Members:
### copy
<pre><code class="language-scala" >def copy(original: TypeBind)(name: String, tpt: Tree)(ctx: Context): TypeBind</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(String, Tree)]</pre></code>

