scala.tasty.reflect.TreeOps
# object LambdaTypeTree

<pre><code class="language-scala" >final object LambdaTypeTree extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(tparams: List[TypeDef], body: Tree)(ctx: Context): LambdaTypeTree</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: LambdaTypeTree)(tparams: List[TypeDef], body: Tree)(ctx: Context): LambdaTypeTree</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(List[TypeDef], Tree)]</pre></code>

