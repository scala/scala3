scala.tasty.reflect.TreeOps
# object DefDef

<pre><code class="language-scala" >final object DefDef extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(symbol: DefDefSymbol, rhsFn: (List[Type]) => (List[List[Term]]) => Option[Term])(ctx: Context): DefDef</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: DefDef)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term])(ctx: Context): DefDef</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(String, List[TypeDef], List[List[ValDef]], TypeTree, Option[Term])]</pre></code>

