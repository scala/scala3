scala.tasty.reflect.TreeOps
# object TypeDef

<pre><code class="language-scala" >final object TypeDef extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(symbol: TypeDefSymbol)(ctx: Context): TypeDef</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: TypeDef)(name: String, rhs: Tree)(ctx: Context): TypeDef</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(String, Tree)]</pre></code>

