scala.internal.quoted
# class TastyTreeExpr

<pre><code class="language-scala" >final class TastyTreeExpr[Tree] extends Expr[Any]</pre></code>
An Expr backed by a tree. Only the current compiler trees are allowed.
These expressions are used for arguments of macros. They contain and actual tree
from the program that is being expanded by the macro.
May contain references to code defined outside this TastyTreeExpr instance.

## Constructors:
<pre><code class="language-scala" >TastyTreeExpr(tree: Tree)</pre></code>

## Concrete Value Members:
### run
<pre><code class="language-scala" >final def run(toolbox: <a href="../../quoted/Toolbox.md">Toolbox</a>): <a href="../../quoted/Expr.md#T">T</a></pre></code>
Evaluate the contents of this expression and return the result.
May throw a FreeVariableError on expressions that came from a macro.

### toString
<pre><code class="language-scala" >override def toString: String</pre></code>

### tree
<pre><code class="language-scala" >val tree: Tree</pre></code>

