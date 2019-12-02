scala.tasty.reflect.TreeOps
# object Applied

<pre><code class="language-scala" >final object Applied extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(tpt: TypeTree, args: List[Tree])(implicit ctx: Context): Applied</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: Applied)(tpt: TypeTree, args: List[Tree])(implicit ctx: Context): Applied</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(TypeTree, List[Tree])]</pre></code>

