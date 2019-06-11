scala.tasty.reflect.TypeOrBoundsOps
# class TypeAPI

<pre><code class="language-scala" >class TypeAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >TypeAPI(self: Type)</pre></code>

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### <:<
<pre><code class="language-scala" >def <:<(that: Type)(ctx: Context): Boolean</pre></code>

### =:=
<pre><code class="language-scala" >def =:=(that: Type)(ctx: Context): Boolean</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### classSymbol
<pre><code class="language-scala" >def classSymbol(ctx: Context): Option[ClassDefSymbol]</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### dealias
<pre><code class="language-scala" >def dealias(ctx: Context): Type</pre></code>
Follow aliases and dereferences LazyRefs, annotated types and instantiated
TypeVars until type is no longer alias type, annotated type, LazyRef,
or instantiated type variable.

### derivesFrom
<pre><code class="language-scala" >def derivesFrom(cls: ClassDefSymbol)(ctx: Context): Boolean</pre></code>
Is this type an instance of a non-bottom subclass of the given class `cls`?

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

### isSingleton
<pre><code class="language-scala" >def isSingleton(ctx: Context): Boolean</pre></code>

### memberType
<pre><code class="language-scala" >def memberType(member: Symbol)(ctx: Context): Type</pre></code>

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

### typeSymbol
<pre><code class="language-scala" >def typeSymbol(ctx: Context): Symbol</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

### widen
<pre><code class="language-scala" >def widen(ctx: Context): Type</pre></code>

