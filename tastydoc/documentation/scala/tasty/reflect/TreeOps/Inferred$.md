scala.tasty.reflect.TreeOps
# object Inferred

<pre><code class="language-scala" >final object Inferred extends Serializable</pre></code>
TypeTree containing an inferred type

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(tpe: Type)(implicit ctx: Context): Inferred</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Boolean</pre></code>
Matches a TypeTree containing an inferred type

