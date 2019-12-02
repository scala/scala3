scala.tasty.reflect.TreeOps
# object This

<pre><code class="language-scala" >final object This extends Serializable</pre></code>
Scala `this` or `this[id]`

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(cls: ClassDefSymbol)(implicit ctx: Context): This</pre></code>
Create a `this[<id: Id]>`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(qual: Option[Id])(implicit ctx: Context): This</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[Option[Id]]</pre></code>
Matches `this[<id: Option[Id]>`

