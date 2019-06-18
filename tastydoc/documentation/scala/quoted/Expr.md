scala.quoted
# class Expr

## Companion object <a href="./Expr$.md">Expr</a>

<pre><code class="language-scala" >sealed abstract class Expr[T]</pre></code>
## Concrete Value Members:
### run
<pre><code class="language-scala" >final def run(implicit toolbox: <a href="./Toolbox.md">Toolbox</a>): <a href="./Expr.md#T">T</a></pre></code>
Evaluate the contents of this expression and return the result.
May throw a FreeVariableError on expressions that came from a macro.

