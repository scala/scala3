scala.tasty.reflect.TreeOps
# object Apply

<pre><code class="language-scala" >final object Apply extends Serializable</pre></code>
Scala parameter application

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(fun: Term, args: List[Term])(ctx: Context): Apply</pre></code>
Create a function application `<fun: Term>(<args: List[Term]>)`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(fun: Term, args: List[Term])(ctx: Context): Apply</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(Term, List[Term])]</pre></code>
Matches a function application `<fun: Term>(<args: List[Term]>)`

