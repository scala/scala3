scala.tasty.reflect.TreeOps
# object TypeSelect

<pre><code class="language-scala" >final object TypeSelect extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(qualifier: Term, name: String)(ctx: Context): TypeSelect</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: TypeSelect)(qualifier: Term, name: String)(ctx: Context): TypeSelect</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(Term, String)]</pre></code>

