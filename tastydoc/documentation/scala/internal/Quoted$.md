scala.internal
# object Quoted

<pre><code class="language-scala" >final object Quoted extends Serializable</pre></code>
## Concrete Type Members:
### patternBindHole
<pre><code class="language-scala" >@compileTimeOnly class <a href="./Quoted$/patternBindHole.md">patternBindHole</a></pre></code>
A splice of a name in a quoted pattern is desugared by wrapping getting this annotation

## Concrete Value Members:
### exprQuote
<pre><code class="language-scala" >@compileTimeOnly def exprQuote[T](x: T): <a href="../quoted/Expr.md">Expr</a>[T]</pre></code>
A term quote is desugared by the compiler into a call to this method

### exprSplice
<pre><code class="language-scala" >@compileTimeOnly def exprSplice[T](x: <a href="../quoted/Expr.md">Expr</a>[T]): T</pre></code>
A term splice is desugared by the compiler into a call to this method

### patternHole
<pre><code class="language-scala" >@compileTimeOnly def patternHole[T]: T</pre></code>
A splice in a quoted pattern is desugared by the compiler into a call to this method

### typeQuote
<pre><code class="language-scala" >@compileTimeOnly def typeQuote[T <: scala.AnyKind]: <a href="../quoted/Type.md">Type</a>[T]</pre></code>
A type quote is desugared by the compiler into a call to this method

