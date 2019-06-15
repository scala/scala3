scala.tasty.reflect.TreeOps
# object TypeApply

<pre><code class="language-scala" >final object TypeApply extends Serializable</pre></code>
Scala type parameter application

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(fun: Term, args: List[TypeTree])(ctx: Context): TypeApply</pre></code>
Create a function type application `<fun: Term>[<args: List[TypeTree]>]`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(fun: Term, args: List[TypeTree])(ctx: Context): TypeApply</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(Term, List[TypeTree])]</pre></code>
Matches a function type application `<fun: Term>[<args: List[TypeTree]>]`

