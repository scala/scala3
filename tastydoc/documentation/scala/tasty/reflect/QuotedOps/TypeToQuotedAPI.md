scala.tasty.reflect.QuotedOps
# class TypeToQuotedAPI

<pre><code class="language-scala" >class TypeToQuotedAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >TypeToQuotedAPI(tpe: Type)</pre></code>

## Concrete Value Members:
### seal
<pre><code class="language-scala" >def seal(implicit ctx: Context): <a href="../../../quoted/Type.md">Type</a>[Nothing <: AnyKind]</pre></code>
Convert `Type` to an `quoted.Type[_]`

