scala.tasty.reflect.QuotedOps
# class QuotedTypeAPI

<pre><code class="language-scala" >class QuotedTypeAPI[T <: scala.AnyKind]</pre></code>
## Constructors:
<pre><code class="language-scala" >QuotedTypeAPI(tpe: <a href="../../../quoted/Type.md">Type</a>[T])</pre></code>

## Concrete Value Members:
### show
<pre><code class="language-scala" >def show(ctx: Context): String</pre></code>
Show a source code like representation of this type

### unseal
<pre><code class="language-scala" >def unseal(ctx: Context): TypeTree</pre></code>
View this expression `quoted.Type[T]` as a `TypeTree`

