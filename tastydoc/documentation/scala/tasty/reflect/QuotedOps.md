scala.tasty.reflect
# trait QuotedOps

<pre><code class="language-scala" >trait QuotedOps extends Core</pre></code>
Extension methods on scala.quoted.{Expr|Type} to convert to scala.tasty.Tasty objects

## Known subclasses:
<a href="./QuotedOps/TypeToQuotedAPI.md">TypeToQuotedAPI</a>, <a href="./QuotedOps/TermToQuotedAPI.md">TermToQuotedAPI</a>, <a href="./QuotedOps/QuotedTypeAPI.md">QuotedTypeAPI</a>, <a href="./QuotedOps/QuotedExprAPI.md">QuotedExprAPI</a>
## Constructors:
<pre><code class="language-scala" >QuotedOps()</pre></code>

## Concrete Type Members:
### QuotedExprAPI
<pre><code class="language-scala" >class <a href="./QuotedOps/QuotedExprAPI.md">QuotedExprAPI</a></pre></code>
### QuotedTypeAPI
<pre><code class="language-scala" >class <a href="./QuotedOps/QuotedTypeAPI.md">QuotedTypeAPI</a></pre></code>
### TermToQuotedAPI
<pre><code class="language-scala" >class <a href="./QuotedOps/TermToQuotedAPI.md">TermToQuotedAPI</a></pre></code>
### TypeToQuotedAPI
<pre><code class="language-scala" >class <a href="./QuotedOps/TypeToQuotedAPI.md">TypeToQuotedAPI</a></pre></code>
## Concrete Value Members:
### QuotedExprAPI
<pre><code class="language-scala" >final implicit def QuotedExprAPI[T](expr: <a href="../../quoted/Expr.md">Expr</a>[T]): QuotedExprAPI[T]</pre></code>

### QuotedTypeAPI
<pre><code class="language-scala" >final implicit def QuotedTypeAPI[T <: scala.AnyKind](tpe: <a href="../../quoted/Type.md">Type</a>[T]): QuotedTypeAPI[T]</pre></code>

### TermToQuotedAPI
<pre><code class="language-scala" >final implicit def TermToQuotedAPI(term: Term): TermToQuotedAPI</pre></code>

### TypeToQuotedAPI
<pre><code class="language-scala" >final implicit def TypeToQuotedAPI(tpe: Type): TypeToQuotedAPI</pre></code>

