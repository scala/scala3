scala.tasty.reflect.TreeOps
# object New

<pre><code class="language-scala" >final object New extends Serializable</pre></code>
Scala `new`

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(tpt: TypeTree)(implicit ctx: Context): New</pre></code>
Create a `new <tpt: TypeTree>`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(tpt: TypeTree)(implicit ctx: Context): New</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[TypeTree]</pre></code>
Matches a `new <tpt: TypeTree>`

