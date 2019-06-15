scala.tasty.reflect.SignatureOps
# object Signature

<pre><code class="language-scala" >final object Signature extends Serializable</pre></code>
Erased (JVM) signatures.

## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply(sig: Signature)(ctx: Context): Option[(List[String], String)]</pre></code>
Matches the erased (JVM) signature and returns its parameters and result type.

