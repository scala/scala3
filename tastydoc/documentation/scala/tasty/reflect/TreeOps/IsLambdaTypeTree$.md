scala.tasty.reflect.TreeOps
# object IsLambdaTypeTree

<pre><code class="language-scala" >final object IsLambdaTypeTree extends Serializable</pre></code>
## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[LambdaTypeTree]</pre></code>
Matches any LambdaTypeTree and returns it

