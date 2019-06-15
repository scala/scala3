scala.tasty.reflect
# trait TreeUtils

<pre><code class="language-scala" >trait TreeUtils extends Core with PatternOps with SymbolOps with TreeOps</pre></code>
Tasty reflect case definition

## Known subclasses:
<a href="./TreeUtils/TreeMap.md">TreeMap</a>, <a href="./TreeUtils/TreeTraverser.md">TreeTraverser</a>, <a href="./TreeUtils/TreeAccumulator.md">TreeAccumulator</a>
## Constructors:
<pre><code class="language-scala" >TreeUtils()</pre></code>

## Abstract Type Members:
### TreeAccumulator
<pre><code class="language-scala" >abstract class <a href="./TreeUtils/TreeAccumulator.md">TreeAccumulator</a></pre></code>
### TreeMap
<pre><code class="language-scala" >abstract class <a href="./TreeUtils/TreeMap.md">TreeMap</a></pre></code>
### TreeTraverser
<pre><code class="language-scala" >abstract class <a href="./TreeUtils/TreeTraverser.md">TreeTraverser</a></pre></code>
## Concrete Value Members:
### AlternativesAPI
<pre><code class="language-scala" >final implicit def AlternativesAPI(alternatives: Alternatives): AlternativesAPI</pre></code>

### AnnotatedAPI
<pre><code class="language-scala" >final implicit def AnnotatedAPI(self: Annotated): AnnotatedAPI</pre></code>

### AppliedAPI
<pre><code class="language-scala" >final implicit def AppliedAPI(self: Applied): AppliedAPI</pre></code>

### ApplyAPI
<pre><code class="language-scala" >final implicit def ApplyAPI(self: Apply): ApplyAPI</pre></code>

### AssignAPI
<pre><code class="language-scala" >final implicit def AssignAPI(self: Assign): AssignAPI</pre></code>

### BindAPI
<pre><code class="language-scala" >final implicit def BindAPI(bind: Bind): BindAPI</pre></code>

### BindSymbolAPI
<pre><code class="language-scala" >final implicit def BindSymbolAPI(self: BindSymbol): BindSymbolAPI</pre></code>

### BlockAPI
<pre><code class="language-scala" >final implicit def BlockAPI(self: Block): BlockAPI</pre></code>

### ByNameAPI
<pre><code class="language-scala" >final implicit def ByNameAPI(self: ByName): ByNameAPI</pre></code>

### CaseDefAPI
<pre><code class="language-scala" >final implicit def CaseDefAPI(caseDef: CaseDef): CaseDefAPI</pre></code>

### ClassDefAPI
<pre><code class="language-scala" >final implicit def ClassDefAPI(self: ClassDef): ClassDefAPI</pre></code>

### ClassDefSymbolAPI
<pre><code class="language-scala" >final implicit def ClassDefSymbolAPI(self: ClassDefSymbol): ClassDefSymbolAPI</pre></code>

### DefDefAPI
<pre><code class="language-scala" >final implicit def DefDefAPI(self: DefDef): DefDefAPI</pre></code>

### DefDefSymbolAPI
<pre><code class="language-scala" >final implicit def DefDefSymbolAPI(self: DefDefSymbol): DefDefSymbolAPI</pre></code>

### DefinitionAPI
<pre><code class="language-scala" >final implicit def DefinitionAPI(self: Definition): DefinitionAPI</pre></code>

### IdentAPI
<pre><code class="language-scala" >final implicit def IdentAPI(self: Ident): IdentAPI</pre></code>

### IfAPI
<pre><code class="language-scala" >final implicit def IfAPI(self: If): IfAPI</pre></code>

### ImplicitMatchAPI
<pre><code class="language-scala" >final implicit def ImplicitMatchAPI(self: ImpliedMatch): ImplicitMatchAPI</pre></code>

### ImportAPI
<pre><code class="language-scala" >final implicit def ImportAPI(self: Import): ImportAPI</pre></code>

### InlinedAPI
<pre><code class="language-scala" >final implicit def InlinedAPI(self: Inlined): InlinedAPI</pre></code>

### LambdaAPI
<pre><code class="language-scala" >final implicit def LambdaAPI(self: Lambda): LambdaAPI</pre></code>

### LambdaTypeTreeAPI
<pre><code class="language-scala" >final implicit def LambdaTypeTreeAPI(self: LambdaTypeTree): LambdaTypeTreeAPI</pre></code>

### LiteralAPI
<pre><code class="language-scala" >final implicit def LiteralAPI(self: Literal): LiteralAPI</pre></code>

### MatchAPI
<pre><code class="language-scala" >final implicit def MatchAPI(self: Match): MatchAPI</pre></code>

### MatchTypeTreeAPI
<pre><code class="language-scala" >final implicit def MatchTypeTreeAPI(self: MatchTypeTree): MatchTypeTreeAPI</pre></code>

### NamedArgAPI
<pre><code class="language-scala" >final implicit def NamedArgAPI(self: NamedArg): NamedArgAPI</pre></code>

### NewAPI
<pre><code class="language-scala" >final implicit def NewAPI(self: New): NewAPI</pre></code>

### PackageClauseAPI
<pre><code class="language-scala" >final implicit def PackageClauseAPI(self: PackageClause): PackageClauseAPI</pre></code>

### PackageDefAPI
<pre><code class="language-scala" >final implicit def PackageDefAPI(self: PackageDef): PackageDefAPI</pre></code>

### PackageDefSymbolAPI
<pre><code class="language-scala" >final implicit def PackageDefSymbolAPI(self: PackageDefSymbol): PackageDefSymbolAPI</pre></code>

### PatternAPI
<pre><code class="language-scala" >final implicit def PatternAPI(self: Pattern): PatternAPI</pre></code>

### ProjectionAPI
<pre><code class="language-scala" >final implicit def ProjectionAPI(self: Projection): ProjectionAPI</pre></code>

### RefinedAPI
<pre><code class="language-scala" >final implicit def RefinedAPI(self: Refined): RefinedAPI</pre></code>

### RepeatedAPI
<pre><code class="language-scala" >final implicit def RepeatedAPI(self: Repeated): RepeatedAPI</pre></code>

### ReturnAPI
<pre><code class="language-scala" >final implicit def ReturnAPI(self: Return): ReturnAPI</pre></code>

### SelectAPI
<pre><code class="language-scala" >final implicit def SelectAPI(self: Select): SelectAPI</pre></code>

### SelectOuterAPI
<pre><code class="language-scala" >final implicit def SelectOuterAPI(self: SelectOuter): SelectOuterAPI</pre></code>

### SingletonAPI
<pre><code class="language-scala" >final implicit def SingletonAPI(self: Singleton): SingletonAPI</pre></code>

### SuperAPI
<pre><code class="language-scala" >final implicit def SuperAPI(self: Super): SuperAPI</pre></code>

### SymbolAPI
<pre><code class="language-scala" >final implicit def SymbolAPI(self: Symbol): SymbolAPI</pre></code>

### TermAPI
<pre><code class="language-scala" >final implicit def TermAPI(self: Term): TermAPI</pre></code>

### ThisAPI
<pre><code class="language-scala" >final implicit def ThisAPI(self: This): ThisAPI</pre></code>

### TreeAPI
<pre><code class="language-scala" >final implicit def TreeAPI(self: Tree): TreeAPI</pre></code>

### TryAPI
<pre><code class="language-scala" >final implicit def TryAPI(self: Try): TryAPI</pre></code>

### TypeApplyAPI
<pre><code class="language-scala" >final implicit def TypeApplyAPI(self: TypeApply): TypeApplyAPI</pre></code>

### TypeBindAPI
<pre><code class="language-scala" >final implicit def TypeBindAPI(self: TypeBind): TypeBindAPI</pre></code>

### TypeBindSymbolAPI
<pre><code class="language-scala" >final implicit def TypeBindSymbolAPI(self: TypeBindSymbol): TypeBindSymbolAPI</pre></code>

### TypeBlockAPI
<pre><code class="language-scala" >final implicit def TypeBlockAPI(self: TypeBlock): TypeBlockAPI</pre></code>

### TypeBoundsTreeAPI
<pre><code class="language-scala" >final implicit def TypeBoundsTreeAPI(self: TypeBoundsTree): TypeBoundsTreeAPI</pre></code>

### TypeCaseDefAPI
<pre><code class="language-scala" >final implicit def TypeCaseDefAPI(caseDef: TypeCaseDef): TypeCaseDefAPI</pre></code>

### TypeDefAPI
<pre><code class="language-scala" >final implicit def TypeDefAPI(self: TypeDef): TypeDefAPI</pre></code>

### TypeDefSymbolAPI
<pre><code class="language-scala" >final implicit def TypeDefSymbolAPI(self: TypeDefSymbol): TypeDefSymbolAPI</pre></code>

### TypeIdentAPI
<pre><code class="language-scala" >final implicit def TypeIdentAPI(self: TypeIdent): TypeIdentAPI</pre></code>

### TypeSelectAPI
<pre><code class="language-scala" >final implicit def TypeSelectAPI(self: TypeSelect): TypeSelectAPI</pre></code>

### TypeTestAPI
<pre><code class="language-scala" >final implicit def TypeTestAPI(typeTest: TypeTest): TypeTestAPI</pre></code>

### TypeTreeAPI
<pre><code class="language-scala" >final implicit def TypeTreeAPI(self: TypeTree): TypeTreeAPI</pre></code>

### TypedAPI
<pre><code class="language-scala" >final implicit def TypedAPI(self: Typed): TypedAPI</pre></code>

### UnapplyAPI
<pre><code class="language-scala" >final implicit def UnapplyAPI(unapply: Unapply): UnapplyAPI</pre></code>

### ValDefAPI
<pre><code class="language-scala" >final implicit def ValDefAPI(self: ValDef): ValDefAPI</pre></code>

### ValDefSymbolAPI
<pre><code class="language-scala" >final implicit def ValDefSymbolAPI(self: ValDefSymbol): ValDefSymbolAPI</pre></code>

### ValueAPI
<pre><code class="language-scala" >final implicit def ValueAPI(value: Value): ValueAPI</pre></code>

### WhileAPI
<pre><code class="language-scala" >final implicit def WhileAPI(self: While): WhileAPI</pre></code>

### WildcardTypeTreeAPI
<pre><code class="language-scala" >final implicit def WildcardTypeTreeAPI(self: WildcardTypeTree): WildcardTypeTreeAPI</pre></code>

