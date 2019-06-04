# Package quoted
## Members:
<pre><code class="language-scala" >final object <a href="./Expr.md">Expr</a></pre></code>
<pre><code class="language-scala" >final val Expr: <a href="./Expr$.md">Expr$</a></pre></code>

<pre><code class="language-scala" >sealed abstract class <a href="./Expr.md">Expr</a></pre></code>
<pre><code class="language-scala" >final object <a href="./QuoteError.md">QuoteError</a></pre></code>
<pre><code class="language-scala" >final val QuoteError: <a href="./QuoteError$.md">QuoteError$</a></pre></code>

<pre><code class="language-scala" >class <a href="./QuoteError.md">QuoteError</a></pre></code>
Throwing this error in the implementation of a macro
will result in a compilation error with the given message.

<pre><code class="language-scala" >final object <a href="./package.md">package</a></pre></code>
<pre><code class="language-scala" >final val package: <a href="./package$.md">package$</a></pre></code>

<pre><code class="language-scala" >final object <a href="./Types.md">Types</a></pre></code>
All implementations of Type[T].
These should never be used directly.

<pre><code class="language-scala" >final val Types: <a href="./Types$.md">Types$</a></pre></code>
All implementations of Type[T].
These should never be used directly.


<pre><code class="language-scala" >final object <a href="./Liftable.md">Liftable</a></pre></code>
Some liftable base types. To be completed with at least all types
that are valid Scala literals. The actual implementation of these
typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
gives an alternative implementation using just the basic staging system.

<pre><code class="language-scala" >final val Liftable: <a href="./Liftable$.md">Liftable$</a></pre></code>
Some liftable base types. To be completed with at least all types
that are valid Scala literals. The actual implementation of these
typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
gives an alternative implementation using just the basic staging system.


<pre><code class="language-scala" >abstract class <a href="./Liftable.md">Liftable</a></pre></code>
A typeclass for types that can be turned to `quoted.Expr[T]`
without going through an explicit `'{...}` operation.

<pre><code class="language-scala" >final object <a href="./Toolbox.md">Toolbox</a></pre></code>
<pre><code class="language-scala" >final val Toolbox: <a href="./Toolbox$.md">Toolbox$</a></pre></code>

<pre><code class="language-scala" >@implicitNotFound trait <a href="./Toolbox.md">Toolbox</a></pre></code>
<pre><code class="language-scala" >final object <a href="./Exprs.md">Exprs</a></pre></code>
All implementations of Expr[T].
These should never be used directly.

<pre><code class="language-scala" >final val Exprs: <a href="./Exprs$.md">Exprs$</a></pre></code>
All implementations of Expr[T].
These should never be used directly.


<pre><code class="language-scala" >final object <a href="./Type.md">Type</a></pre></code>
Some basic type tags, currently incomplete

<pre><code class="language-scala" >final val Type: <a href="./Type$.md">Type$</a></pre></code>
Some basic type tags, currently incomplete


<pre><code class="language-scala" >sealed abstract class <a href="./Type.md">Type</a></pre></code>
<pre><code class="language-scala" >package <a href="./matching/matching.md">matching</a></pre></code>