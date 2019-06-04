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
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### $asInstanceOf$
<pre><code class="language-scala" >final def $asInstanceOf$[X0]: X0</pre></code>

### $isInstanceOf$
<pre><code class="language-scala" >final def $isInstanceOf$[X0]: Boolean</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): Boolean</pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: Any): Boolean</pre></code>

### finalize
<pre><code class="language-scala" >protected def finalize(): Unit</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### liftedExpr
<pre><code class="language-scala" >def liftedExpr[T](value: T): <a href="../../quoted/Exprs$/LiftedExpr.md">LiftedExpr</a>[T]</pre></code>
Lift the `value` to an `Expr` tree.
Values can only be of type Boolean, Byte, Short, Char, Int, Long, Float, Double, Unit, String, Null or Class.

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): Boolean</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### unpickleExpr
<pre><code class="language-scala" >def unpickleExpr[T](repr: Pickled, args: Seq[Any]): <a href="../../quoted/Expr.md">Expr</a>[T]</pre></code>
Unpickle `repr` which represents a pickled `Expr` tree,
replacing splice nodes with `args`

### unpickleType
<pre><code class="language-scala" >def unpickleType[T](repr: Pickled, args: Seq[Any]): <a href="../../quoted/Type.md">Type</a>[T]</pre></code>
Unpickle `repr` which represents a pickled `Type` tree,
replacing splice nodes with `args`

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

### writeReplace
<pre><code class="language-scala" >private def writeReplace(): AnyRef</pre></code>

