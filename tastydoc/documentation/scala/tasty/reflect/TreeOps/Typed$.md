scala.tasty.reflect.TreeOps
# object Typed

<pre><code class="language-scala" >final object Typed extends Serializable</pre></code>
Scala ascription `x: T`

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(expr: Term, tpt: TypeTree)(ctx: Context): Typed</pre></code>
Create a type ascription `<x: Term>: <tpt: TypeTree>`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(expr: Term, tpt: TypeTree)(ctx: Context): Typed</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(Term, TypeTree)]</pre></code>
Matches `<expr: Term>: <tpt: TypeTree>`

