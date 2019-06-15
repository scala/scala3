scala.tasty.reflect.TreeOps
# object Select

<pre><code class="language-scala" >final object Select extends Serializable</pre></code>
Scala term selection

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### apply
<pre><code class="language-scala" >def apply(qualifier: Term, symbol: Symbol)(ctx: Context): Select</pre></code>
Select a term member by symbol

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### copy
<pre><code class="language-scala" >def copy(original: Tree)(qualifier: Term, name: String)(ctx: Context): Select</pre></code>

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

### overloaded
<pre><code class="language-scala" >def overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term])(ctx: Context): Apply</pre></code>
Call an overloaded method with the given type and term parameters

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(Term, String)]</pre></code>
Matches `<qualifier: Term>.<name: String>`

### unique
<pre><code class="language-scala" >def unique(qualifier: Term, name: String)(ctx: Context): Select</pre></code>
Select a field or a non-overloaded method by name

***Note*** The method will produce an assertion error if the selected
      method is overloaded. The method `overloaded` should be used
      in that case.

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

