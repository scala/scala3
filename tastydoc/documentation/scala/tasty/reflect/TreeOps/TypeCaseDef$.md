scala.tasty.reflect.TreeOps
# object TypeCaseDef

<pre><code class="language-scala" >final object TypeCaseDef extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(pattern: TypeTree, rhs: TypeTree)(implicit ctx: Context): TypeCaseDef</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: TypeCaseDef)(pattern: TypeTree, rhs: TypeTree)(implicit ctx: Context): TypeCaseDef</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(TypeTree, TypeTree)]</pre></code>

