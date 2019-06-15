scala.quoted
# object Expr

## Companion class <a href="./Expr.md">Expr</a>

<pre><code class="language-scala" >final object Expr extends Serializable</pre></code>
## Known subclasses:
<a href="./Expr$/AsContextualFunction.md">AsContextualFunction</a>, <a href="./Expr$/AsFunction.md">AsFunction</a>, <a href="./Expr$/ExprOps.md">ExprOps</a>
## Concrete Type Members:
### TupleOfExpr
<pre><code class="language-scala" >type TupleOfExpr: [Tup >: scala.Nothing <: scala.Tuple] => scala.Tuple.Map[Tup, [+T >: scala.Nothing <: scala.Any] => scala.quoted.Expr[+T]]</pre></code>
Converts a tuple `(T1, ..., Tn)` to `(Expr[T1], ..., Expr[Tn])`


### AsContextualFunction
<pre><code class="language-scala" >class <a href="./Expr$/AsContextualFunction.md">AsContextualFunction</a></pre></code>
### AsFunction
<pre><code class="language-scala" >class <a href="./Expr$/AsFunction.md">AsFunction</a></pre></code>
### ExprOps
<pre><code class="language-scala" >class <a href="./Expr$/ExprOps.md">ExprOps</a></pre></code>
## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

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

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

