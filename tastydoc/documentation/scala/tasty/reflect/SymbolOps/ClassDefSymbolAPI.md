scala.tasty.reflect.SymbolOps
# class ClassDefSymbolAPI

<pre><code class="language-scala" >class ClassDefSymbolAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >ClassDefSymbolAPI(self: ClassDefSymbol)</pre></code>

## Concrete Value Members:
### caseFields
<pre><code class="language-scala" >def caseFields(ctx: Context): List[ValDefSymbol]</pre></code>
Fields of a case class type -- only the ones declared in primary constructor

### classMethod
<pre><code class="language-scala" >def classMethod(name: String)(ctx: Context): List[DefDefSymbol]</pre></code>
Get non-private named methods defined directly inside the class

### classMethods
<pre><code class="language-scala" >def classMethods(ctx: Context): List[DefDefSymbol]</pre></code>
Get all non-private methods defined directly inside the class, exluding constructors

### companionClass
<pre><code class="language-scala" >def companionClass(ctx: Context): Option[ClassDefSymbol]</pre></code>
The class symbol of the companion module class

### companionModule
<pre><code class="language-scala" >def companionModule(ctx: Context): Option[ValDefSymbol]</pre></code>
The symbol of the companion module

### field
<pre><code class="language-scala" >def field(name: String)(ctx: Context): Option[Symbol]</pre></code>
Field with the given name directly declared in the class

### fields
<pre><code class="language-scala" >def fields(ctx: Context): List[Symbol]</pre></code>
Fields directly declared in the class

### method
<pre><code class="language-scala" >def method(name: String)(ctx: Context): List[DefDefSymbol]</pre></code>
Get named non-private methods declared or inherited

### methods
<pre><code class="language-scala" >def methods(ctx: Context): List[DefDefSymbol]</pre></code>
Get all non-private methods declared or inherited

### moduleClass
<pre><code class="language-scala" >def moduleClass(ctx: Context): Option[Symbol]</pre></code>
The symbol of the class of the companion module

### tree
<pre><code class="language-scala" >def tree(ctx: Context): ClassDef</pre></code>
ClassDef tree of this defintion

