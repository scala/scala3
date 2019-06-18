scala.tasty.reflect.TreeOps
# object Assign

<pre><code class="language-scala" >final object Assign extends Serializable</pre></code>
Scala assign `x = y`

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(lhs: Term, rhs: Term)(implicit ctx: Context): Assign</pre></code>
Create an assignment `<lhs: Term> = <rhs: Term>`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(lhs: Term, rhs: Term)(implicit ctx: Context): Assign</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)]</pre></code>
Matches an assignment `<lhs: Term> = <rhs: Term>`

