scala.tasty.reflect.TreeOps
# object Match

<pre><code class="language-scala" >final object Match extends Serializable</pre></code>
Scala `match` term

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match</pre></code>
Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef])]</pre></code>
Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }`

