scala.tasty.reflect.TreeOps
# object TypeBlock

<pre><code class="language-scala" >final object TypeBlock extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: TypeBlock)(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(List[TypeDef], TypeTree)]</pre></code>

