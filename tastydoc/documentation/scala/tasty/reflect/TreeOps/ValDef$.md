scala.tasty.reflect.TreeOps
# object ValDef

<pre><code class="language-scala" >final object ValDef extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(symbol: ValDefSymbol, rhs: Option[Term])(implicit ctx: Context): ValDef</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: ValDef)(name: String, tpt: TypeTree, rhs: Option[Term])(implicit ctx: Context): ValDef</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])]</pre></code>

