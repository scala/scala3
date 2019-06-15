scala.tasty.reflect.TreeOps
# object Literal

<pre><code class="language-scala" >final object Literal extends Serializable</pre></code>
Scala literal constant

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(constant: Constant)(ctx: Context): Literal</pre></code>
Create a literal constant

### copy
<pre><code class="language-scala" >def copy(original: Tree)(constant: Constant)(ctx: Context): Literal</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[Constant]</pre></code>
Matches a literal constant

