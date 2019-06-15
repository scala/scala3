scala.tasty.reflect.TreeOps
# object Try

<pre><code class="language-scala" >final object Try extends Serializable</pre></code>
Scala `try`/`catch`/`finally` term

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(ctx: Context): Try</pre></code>
Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(ctx: Context): Try</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(Term, List[CaseDef], Option[Term])]</pre></code>
Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>`

