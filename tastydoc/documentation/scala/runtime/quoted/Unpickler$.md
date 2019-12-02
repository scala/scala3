scala.runtime.quoted
# object Unpickler

<pre><code class="language-scala" >final object Unpickler extends Serializable</pre></code>
Provides methods to unpickle `Expr` and `Type` trees.

## Concrete Type Members:
### Pickled
<pre><code class="language-scala" >type Pickled: List[String]</pre></code>
Representation of pickled trees. For now a List[String],
but it should be changed to some kind of TASTY bundle.


## Concrete Value Members:
### liftedExpr
<pre><code class="language-scala" >def liftedExpr[T](value: T): <a href="../../quoted/Expr.md">Expr</a>[T]</pre></code>
Lift the `value` to an `Expr` tree.
Values can only be of type Boolean, Byte, Short, Char, Int, Long, Float, Double, Unit, String, Null or Class.

### unpickleExpr
<pre><code class="language-scala" >def unpickleExpr[T](repr: Pickled, args: Seq[Any]): <a href="../../quoted/Expr.md">Expr</a>[T]</pre></code>
Unpickle `repr` which represents a pickled `Expr` tree,
replacing splice nodes with `args`

### unpickleType
<pre><code class="language-scala" >def unpickleType[T](repr: Pickled, args: Seq[Any]): <a href="../../quoted/Type.md">Type</a>[T]</pre></code>
Unpickle `repr` which represents a pickled `Type` tree,
replacing splice nodes with `args`

