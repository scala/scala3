scala.tasty.reflect.SymbolOps
# class ClassDefSymbolAPI

<pre><code class="language-scala" >class ClassDefSymbolAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >ClassDefSymbolAPI(self: ClassDefSymbol)</pre></code>

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### caseFields
<pre><code class="language-scala" >def caseFields(ctx: Context): List[ValDefSymbol]</pre></code>
Fields of a case class type -- only the ones declared in primary constructor

### classMethod
<pre><code class="language-scala" >def classMethod(name: String)(ctx: Context): List[DefDefSymbol]</pre></code>
Get non-private named methods defined directly inside the class

### classMethods
<pre><code class="language-scala" >def classMethods(ctx: Context): List[DefDefSymbol]</pre></code>
Get all non-private methods defined directly inside the class, exluding constructors

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### companionClass
<pre><code class="language-scala" >def companionClass(ctx: Context): Option[ClassDefSymbol]</pre></code>
The class symbol of the companion module class

### companionModule
<pre><code class="language-scala" >def companionModule(ctx: Context): Option[ValDefSymbol]</pre></code>
The symbol of the companion module

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): Boolean</pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: Any): Boolean</pre></code>

### field
<pre><code class="language-scala" >def field(name: String)(ctx: Context): Option[Symbol]</pre></code>
Field with the given name directly declared in the class

### fields
<pre><code class="language-scala" >def fields(ctx: Context): List[Symbol]</pre></code>
Fields directly declared in the class

### finalize
<pre><code class="language-scala" >protected def finalize(): Unit</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### method
<pre><code class="language-scala" >def method(name: String)(ctx: Context): List[DefDefSymbol]</pre></code>
Get named non-private methods declared or inherited

### methods
<pre><code class="language-scala" >def methods(ctx: Context): List[DefDefSymbol]</pre></code>
Get all non-private methods declared or inherited

### moduleClass
<pre><code class="language-scala" >def moduleClass(ctx: Context): Option[Symbol]</pre></code>
The symbol of the class of the companion module

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

### tree
<pre><code class="language-scala" >def tree(ctx: Context): ClassDef</pre></code>
ClassDef tree of this defintion

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

