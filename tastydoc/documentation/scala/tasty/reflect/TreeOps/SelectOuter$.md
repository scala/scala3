scala.tasty.reflect.TreeOps
# object SelectOuter

<pre><code class="language-scala" >final object SelectOuter extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(qualifier: Term, name: String, levels: Int)(implicit ctx: Context): SelectOuter</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: Tree)(qualifier: Term, name: String, levels: Int)(implicit ctx: Context): SelectOuter</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Int, Type)]</pre></code>

