scala.tasty.reflect.TreeOps
# object PackageClause

<pre><code class="language-scala" >final object PackageClause extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(pid: Ref, stats: List[Tree])(implicit ctx: Context): PackageClause</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: PackageClause)(pid: Ref, stats: List[Tree])(implicit ctx: Context): PackageClause</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(Ref, List[Tree])]</pre></code>

