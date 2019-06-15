scala.tasty.reflect.TreeOps
# object Super

<pre><code class="language-scala" >final object Super extends Serializable</pre></code>
Scala `x.super` or `x.super[id]`

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(qual: Term, mix: Option[Id])(ctx: Context): Super</pre></code>
Creates a `<qualifier: Term>.super[<id: Option[Id]>`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(qual: Term, mix: Option[Id])(ctx: Context): Super</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(Term, Option[Id])]</pre></code>
Matches a `<qualifier: Term>.super[<id: Option[Id]>`

