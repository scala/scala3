scala.tasty.reflect.TreeOps
# object Block

<pre><code class="language-scala" >final object Block extends Serializable</pre></code>
Scala code block `{ stat0; ...; statN; expr }` term

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(stats: List[Statement], expr: Term)(ctx: Context): Block</pre></code>
Creates a block `{ <statements: List[Statement]>; <expr: Term> }`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(stats: List[Statement], expr: Term)(ctx: Context): Block</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(List[Statement], Term)]</pre></code>
Matches a block `{ <statements: List[Statement]>; <expr: Term> }`

