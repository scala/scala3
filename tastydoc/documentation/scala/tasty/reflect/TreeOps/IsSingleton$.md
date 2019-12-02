scala.tasty.reflect.TreeOps
# object IsSingleton

<pre><code class="language-scala" >final object IsSingleton extends Serializable</pre></code>
## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[Singleton]</pre></code>
Matches any Singleton and returns it

