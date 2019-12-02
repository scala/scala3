scala.quoted
# object QuoteError

## Companion class <a href="./QuoteError.md">QuoteError</a>

<pre><code class="language-scala" >final object QuoteError extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(message: => String, from: <a href="./Expr.md">Expr</a>[Nothing <: Any]): Nothing</pre></code>
Throws a QuoteError with the given message at the position of the `from` expression

### apply
<pre><code class="language-scala" >def apply(message: => String): Nothing</pre></code>
Throws a QuoteError with the given message

