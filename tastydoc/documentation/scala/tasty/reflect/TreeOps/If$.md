scala.tasty.reflect.TreeOps
# object If

<pre><code class="language-scala" >final object If extends Serializable</pre></code>
Scala `if`/`else` term

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If</pre></code>
Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term, Term)]</pre></code>
Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>`

