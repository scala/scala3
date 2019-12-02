scala.tasty.Reflection
# object typing

<pre><code class="language-scala" >final object typing extends Serializable</pre></code>
## Concrete Value Members:
### typeChecks
<pre><code class="language-scala" >def typeChecks(code: String)(implicit ctx: <a href="./Context.md">Context</a>): Boolean</pre></code>
Whether the code type checks in the given context?

***return*** false if the code has syntax error or type error in the given context, otherwise returns true.
The code should be a sequence of expressions or statements that may appear in a block.

***code*** The code to be type checked

