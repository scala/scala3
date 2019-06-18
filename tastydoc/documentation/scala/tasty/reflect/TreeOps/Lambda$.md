scala.tasty.reflect.TreeOps
# object Lambda

<pre><code class="language-scala" >final object Lambda extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(meth: Term, tpt: Option[TypeTree])(implicit ctx: Context): Lambda</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: Tree)(meth: Tree, tpt: Option[TypeTree])(implicit ctx: Context): Lambda</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[TypeTree])]</pre></code>

