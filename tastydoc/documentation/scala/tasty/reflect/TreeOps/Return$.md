scala.tasty.reflect.TreeOps
# object Return

<pre><code class="language-scala" >final object Return extends Serializable</pre></code>
Scala local `return`

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(expr: Term)(ctx: Context): Return</pre></code>
Creates `return <expr: Term>`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(expr: Term)(ctx: Context): Return</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[Term]</pre></code>
Matches `return <expr: Term>`

