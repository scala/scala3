scala.tasty.reflect.TreeOps
# object While

<pre><code class="language-scala" >final object While extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(cond: Term, body: Term)(ctx: Context): While</pre></code>
Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>)

### copy
<pre><code class="language-scala" >def copy(original: Tree)(cond: Term, body: Term)(ctx: Context): While</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(Term, Term)]</pre></code>
Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>)

