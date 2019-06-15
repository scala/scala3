scala.tasty.reflect.TreeOps
# object Annotated

<pre><code class="language-scala" >final object Annotated extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(arg: TypeTree, annotation: Term)(ctx: Context): Annotated</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: Annotated)(arg: TypeTree, annotation: Term)(ctx: Context): Annotated</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(TypeTree, Term)]</pre></code>

