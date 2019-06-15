# Package quoted
## Members:
<pre><code class="language-scala" >final object <a href="./Matcher$.md">Matcher</a></pre></code>
<pre><code class="language-scala" >final val Matcher: Matcher</pre></code>

<pre><code class="language-scala" >final class <a href="./TastyExpr.md">TastyExpr</a></pre></code>
An Expr backed by a pickled TASTY tree

<pre><code class="language-scala" >final class <a href="./TaggedType.md">TaggedType</a></pre></code>
An Type backed by a value

<pre><code class="language-scala" >final class <a href="./TastyTreeExpr.md">TastyTreeExpr</a></pre></code>
An Expr backed by a tree. Only the current compiler trees are allowed.
These expressions are used for arguments of macros. They contain and actual tree
from the program that is being expanded by the macro.
May contain references to code defined outside this TastyTreeExpr instance.

<pre><code class="language-scala" >final class <a href="./FunctionAppliedTo.md">FunctionAppliedTo</a></pre></code>
An Expr representing `'{($f).apply($x1, ..., $xn)}` but it is beta-reduced when the closure is known

<pre><code class="language-scala" >final class <a href="./LiftedExpr.md">LiftedExpr</a></pre></code>
An Expr backed by a lifted value.
Values can only be of type Boolean, Byte, Short, Char, Int, Long, Float, Double, Unit, String or Null.

<pre><code class="language-scala" >final class <a href="./TastyType.md">TastyType</a></pre></code>
A Type backed by a pickled TASTY tree

<pre><code class="language-scala" >final class <a href="./TreeType.md">TreeType</a></pre></code>
An Type backed by a tree

