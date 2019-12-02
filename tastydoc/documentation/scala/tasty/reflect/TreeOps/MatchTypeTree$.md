scala.tasty.reflect.TreeOps
# object MatchTypeTree

<pre><code class="language-scala" >final object MatchTypeTree extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): MatchTypeTree</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: MatchTypeTree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): MatchTypeTree</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])]</pre></code>

