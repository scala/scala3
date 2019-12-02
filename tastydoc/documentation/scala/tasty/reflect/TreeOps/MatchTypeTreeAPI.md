scala.tasty.reflect.TreeOps
# class MatchTypeTreeAPI

<pre><code class="language-scala" >class MatchTypeTreeAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >MatchTypeTreeAPI(self: MatchTypeTree)</pre></code>

## Concrete Value Members:
### bound
<pre><code class="language-scala" >def bound(implicit ctx: Context): Option[TypeTree]</pre></code>

### cases
<pre><code class="language-scala" >def cases(implicit ctx: Context): List[TypeCaseDef]</pre></code>

### selector
<pre><code class="language-scala" >def selector(implicit ctx: Context): TypeTree</pre></code>

