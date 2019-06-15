scala.tasty.reflect.TreeOps
# object Import

<pre><code class="language-scala" >final object Import extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(importImplied: Boolean, expr: Term, selectors: List[ImportSelector])(ctx: Context): Import</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: Import)(importImplied: Boolean, expr: Term, selectors: List[ImportSelector])(ctx: Context): Import</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(Boolean, Term, List[ImportSelector])]</pre></code>

