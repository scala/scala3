scala.tasty.reflect.SymbolOps
# class SymbolAPI

<pre><code class="language-scala" >class SymbolAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >SymbolAPI(self: Symbol)</pre></code>

## Concrete Value Members:
### annots
<pre><code class="language-scala" >def annots(implicit ctx: Context): List[Term]</pre></code>
Annotations attached to this symbol

### asBindDef
<pre><code class="language-scala" >def asBindDef(implicit ctx: Context): BindSymbol</pre></code>
Unsafe cast as to BindSymbol. Use IsBindSymbol to safely check and cast to BindSymbol

### asClassDef
<pre><code class="language-scala" >def asClassDef(implicit ctx: Context): ClassDefSymbol</pre></code>
Unsafe cast as to ClassSymbol. Use IsClassDefSymbol to safely check and cast to ClassSymbol

### asDefDef
<pre><code class="language-scala" >def asDefDef(implicit ctx: Context): DefDefSymbol</pre></code>
Unsafe cast as to DefSymbol. Use IsDefDefSymbol to safely check and cast to DefSymbol

### asPackageDef
<pre><code class="language-scala" >def asPackageDef(implicit ctx: Context): PackageDefSymbol</pre></code>
Unsafe cast as to PackageSymbol. Use IsPackageSymbol to safely check and cast to PackageSymbol

### asTypeDef
<pre><code class="language-scala" >def asTypeDef(implicit ctx: Context): TypeDefSymbol</pre></code>
Unsafe cast as to TypeSymbol. Use IsTypeDefSymbol to safely check and cast to TypeSymbol

### asValDef
<pre><code class="language-scala" >def asValDef(implicit ctx: Context): ValDefSymbol</pre></code>
Unsafe cast as to ValSymbol. Use IsValDefSymbol to safely check and cast to ValSymbol

### comment
<pre><code class="language-scala" >def comment(implicit ctx: Context): Option[Comment]</pre></code>
The comment for this symbol, if any

### flags
<pre><code class="language-scala" >def flags(implicit ctx: Context): Flags</pre></code>
Flags of this symbol

### fullName
<pre><code class="language-scala" >def fullName(implicit ctx: Context): String</pre></code>
The full name of this symbol up to the root package

### isAbstractType
<pre><code class="language-scala" >def isAbstractType(implicit ctx: Context): Boolean</pre></code>

### isAliasType
<pre><code class="language-scala" >def isAliasType(implicit ctx: Context): Boolean</pre></code>

### isAnonymousClass
<pre><code class="language-scala" >def isAnonymousClass(implicit ctx: Context): Boolean</pre></code>

### isAnonymousFunction
<pre><code class="language-scala" >def isAnonymousFunction(implicit ctx: Context): Boolean</pre></code>

### isClassConstructor
<pre><code class="language-scala" >def isClassConstructor(implicit ctx: Context): Boolean</pre></code>

### isDefinedInCurrentRun
<pre><code class="language-scala" >def isDefinedInCurrentRun(implicit ctx: Context): Boolean</pre></code>

### isLocalDummy
<pre><code class="language-scala" >def isLocalDummy(implicit ctx: Context): Boolean</pre></code>

### isRefinementClass
<pre><code class="language-scala" >def isRefinementClass(implicit ctx: Context): Boolean</pre></code>

### isTerm
<pre><code class="language-scala" >def isTerm(implicit ctx: Context): Boolean</pre></code>

### isType
<pre><code class="language-scala" >def isType(implicit ctx: Context): Boolean</pre></code>

### localContext
<pre><code class="language-scala" >def localContext(implicit ctx: Context): Context</pre></code>

### name
<pre><code class="language-scala" >def name(implicit ctx: Context): String</pre></code>
The name of this symbol

### owner
<pre><code class="language-scala" >def owner(implicit ctx: Context): Symbol</pre></code>
Owner of this symbol. The owner is the symbol in which this symbol is defined

### pos
<pre><code class="language-scala" >def pos(implicit ctx: Context): Position</pre></code>
The position of this symbol

### privateWithin
<pre><code class="language-scala" >def privateWithin(implicit ctx: Context): Option[Type]</pre></code>
This symbol is private within the resulting type

### protectedWithin
<pre><code class="language-scala" >def protectedWithin(implicit ctx: Context): Option[Type]</pre></code>
This symbol is protected within the resulting type

