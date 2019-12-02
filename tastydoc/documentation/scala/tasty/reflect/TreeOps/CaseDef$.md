scala.tasty.reflect.TreeOps
# object CaseDef

<pre><code class="language-scala" >final object CaseDef extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(pattern: Pattern, guard: Option[Term], rhs: Term)(implicit ctx: Context): CaseDef</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: CaseDef)(pattern: Pattern, guard: Option[Term], rhs: Term)(implicit ctx: Context): CaseDef</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(Pattern, Option[Term], Term)]</pre></code>

