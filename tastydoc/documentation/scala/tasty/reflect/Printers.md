scala.tasty.reflect
# trait Printers

<pre><code class="language-scala" >trait Printers extends Core with ConstantOps with FlagsOps with IdOps with ImportSelectorOps with PatternOps with PositionOps with SignatureOps with StandardDefinitions with SymbolOps with TreeOps with TypeOrBoundsOps</pre></code>
## Known subclasses:
<a href="./Printers/SourceCodePrinter.md">SourceCodePrinter</a>, <a href="./Printers/ExtractorsPrinter.md">ExtractorsPrinter</a>, <a href="./Printers/Printer.md">Printer</a>, <a href="./Printers/FlagsShowDeco.md">FlagsShowDeco</a>, <a href="./Printers/SymbolShowDeco.md">SymbolShowDeco</a>, <a href="./Printers/ConstantShowDeco.md">ConstantShowDeco</a>, <a href="./Printers/PatternShowDeco.md">PatternShowDeco</a>, <a href="./Printers/TypeOrBoundsShowDeco.md">TypeOrBoundsShowDeco</a>, <a href="./Printers/TreeShowDeco.md">TreeShowDeco</a>
## Constructors:
<pre><code class="language-scala" >Printers()</pre></code>

## Abstract Type Members:
### Printer
<pre><code class="language-scala" >abstract class <a href="./Printers/Printer.md">Printer</a></pre></code>
## Concrete Type Members:
### ConstantShowDeco
<pre><code class="language-scala" >class <a href="./Printers/ConstantShowDeco.md">ConstantShowDeco</a></pre></code>
Adds `show` as an extension method of a `Constant`

### ExtractorsPrinter
<pre><code class="language-scala" >class <a href="./Printers/ExtractorsPrinter.md">ExtractorsPrinter</a></pre></code>
### FlagsShowDeco
<pre><code class="language-scala" >class <a href="./Printers/FlagsShowDeco.md">FlagsShowDeco</a></pre></code>
Adds `show` as an extension method of a `Flags`

### PatternShowDeco
<pre><code class="language-scala" >class <a href="./Printers/PatternShowDeco.md">PatternShowDeco</a></pre></code>
Adds `show` as an extension method of a `Pattern`

### SourceCodePrinter
<pre><code class="language-scala" >class <a href="./Printers/SourceCodePrinter.md">SourceCodePrinter</a></pre></code>
### SymbolShowDeco
<pre><code class="language-scala" >class <a href="./Printers/SymbolShowDeco.md">SymbolShowDeco</a></pre></code>
Adds `show` as an extension method of a `Symbol`

### TreeShowDeco
<pre><code class="language-scala" >class <a href="./Printers/TreeShowDeco.md">TreeShowDeco</a></pre></code>
Adds `show` as an extension method of a `Tree`

### TypeOrBoundsShowDeco
<pre><code class="language-scala" >class <a href="./Printers/TypeOrBoundsShowDeco.md">TypeOrBoundsShowDeco</a></pre></code>
Adds `show` as an extension method of a `TypeOrBounds`

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

### ConstantAPI
<pre><code class="language-scala" >final implicit def ConstantAPI(const: Constant): ConstantAPI</pre></code>

### ConstantShowDeco
<pre><code class="language-scala" >final implicit def ConstantShowDeco(const: Constant): ConstantShowDeco</pre></code>
Adds `show` as an extension method of a `Constant`

### DefDefAPI
<pre><code class="language-scala" >final implicit def DefDefAPI(self: DefDef): DefDefAPI</pre></code>

### DefDefSymbolAPI
<pre><code class="language-scala" >final implicit def DefDefSymbolAPI(self: DefDefSymbol): DefDefSymbolAPI</pre></code>

### DefinitionAPI
<pre><code class="language-scala" >final implicit def DefinitionAPI(self: Definition): DefinitionAPI</pre></code>

### FlagsAPI
<pre><code class="language-scala" >final implicit def FlagsAPI(self: Flags): FlagsAPI</pre></code>

### FlagsShowDeco
<pre><code class="language-scala" >final implicit def FlagsShowDeco(flags: Flags): FlagsShowDeco</pre></code>
Adds `show` as an extension method of a `Flags`

### IdAPI
<pre><code class="language-scala" >final implicit def IdAPI(id: Id): IdAPI</pre></code>

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

### OmitSelectorAPI
<pre><code class="language-scala" >final implicit def OmitSelectorAPI(self: OmitSelector): OmitSelectorAPI</pre></code>

### PackageClauseAPI
<pre><code class="language-scala" >final implicit def PackageClauseAPI(self: PackageClause): PackageClauseAPI</pre></code>

### PackageDefAPI
<pre><code class="language-scala" >final implicit def PackageDefAPI(self: PackageDef): PackageDefAPI</pre></code>

### PackageDefSymbolAPI
<pre><code class="language-scala" >final implicit def PackageDefSymbolAPI(self: PackageDefSymbol): PackageDefSymbolAPI</pre></code>

### PatternAPI
<pre><code class="language-scala" >final implicit def PatternAPI(self: Pattern): PatternAPI</pre></code>

### PatternShowDeco
<pre><code class="language-scala" >final implicit def PatternShowDeco(pattern: Pattern): PatternShowDeco</pre></code>
Adds `show` as an extension method of a `Pattern`

### PositionAPI
<pre><code class="language-scala" >final implicit def PositionAPI(pos: Position): PositionAPI</pre></code>

### ProjectionAPI
<pre><code class="language-scala" >final implicit def ProjectionAPI(self: Projection): ProjectionAPI</pre></code>

### RefinedAPI
<pre><code class="language-scala" >final implicit def RefinedAPI(self: Refined): RefinedAPI</pre></code>

### RenameSelectorAPI
<pre><code class="language-scala" >final implicit def RenameSelectorAPI(self: RenameSelector): RenameSelectorAPI</pre></code>

### RepeatedAPI
<pre><code class="language-scala" >final implicit def RepeatedAPI(self: Repeated): RepeatedAPI</pre></code>

### ReturnAPI
<pre><code class="language-scala" >final implicit def ReturnAPI(self: Return): ReturnAPI</pre></code>

### SelectAPI
<pre><code class="language-scala" >final implicit def SelectAPI(self: Select): SelectAPI</pre></code>

### SelectOuterAPI
<pre><code class="language-scala" >final implicit def SelectOuterAPI(self: SelectOuter): SelectOuterAPI</pre></code>

### SignatureAPI
<pre><code class="language-scala" >final implicit def SignatureAPI(sig: Signature): SignatureAPI</pre></code>

### SimpleSelectorAPI
<pre><code class="language-scala" >final implicit def SimpleSelectorAPI(self: SimpleSelector): SimpleSelectorAPI</pre></code>

### SingletonAPI
<pre><code class="language-scala" >final implicit def SingletonAPI(self: Singleton): SingletonAPI</pre></code>

### SourceFileAPI
<pre><code class="language-scala" >final implicit def SourceFileAPI(sourceFile: SourceFile): SourceFileAPI</pre></code>

### SuperAPI
<pre><code class="language-scala" >final implicit def SuperAPI(self: Super): SuperAPI</pre></code>

### SymbolAPI
<pre><code class="language-scala" >final implicit def SymbolAPI(self: Symbol): SymbolAPI</pre></code>

### SymbolShowDeco
<pre><code class="language-scala" >final implicit def SymbolShowDeco(symbol: Symbol): SymbolShowDeco</pre></code>
Adds `show` as an extension method of a `Symbol`

### TermAPI
<pre><code class="language-scala" >final implicit def TermAPI(self: Term): TermAPI</pre></code>

### ThisAPI
<pre><code class="language-scala" >final implicit def ThisAPI(self: This): ThisAPI</pre></code>

### TreeAPI
<pre><code class="language-scala" >final implicit def TreeAPI(self: Tree): TreeAPI</pre></code>

### TreeShowDeco
<pre><code class="language-scala" >final implicit def TreeShowDeco(tree: Tree): TreeShowDeco</pre></code>
Adds `show` as an extension method of a `Tree`

### TryAPI
<pre><code class="language-scala" >final implicit def TryAPI(self: Try): TryAPI</pre></code>

### TypeAPI
<pre><code class="language-scala" >final implicit def TypeAPI(self: Type): TypeAPI</pre></code>

### TypeApplyAPI
<pre><code class="language-scala" >final implicit def TypeApplyAPI(self: TypeApply): TypeApplyAPI</pre></code>

### TypeBindAPI
<pre><code class="language-scala" >final implicit def TypeBindAPI(self: TypeBind): TypeBindAPI</pre></code>

### TypeBindSymbolAPI
<pre><code class="language-scala" >final implicit def TypeBindSymbolAPI(self: TypeBindSymbol): TypeBindSymbolAPI</pre></code>

### TypeBlockAPI
<pre><code class="language-scala" >final implicit def TypeBlockAPI(self: TypeBlock): TypeBlockAPI</pre></code>

### TypeBoundsAPI
<pre><code class="language-scala" >final implicit def TypeBoundsAPI(self: TypeBounds): TypeBoundsAPI</pre></code>

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

### TypeOrBoundsShowDeco
<pre><code class="language-scala" >final implicit def TypeOrBoundsShowDeco(tpe: TypeOrBounds): TypeOrBoundsShowDeco</pre></code>
Adds `show` as an extension method of a `TypeOrBounds`

### TypeSelectAPI
<pre><code class="language-scala" >final implicit def TypeSelectAPI(self: TypeSelect): TypeSelectAPI</pre></code>

### TypeTestAPI
<pre><code class="language-scala" >final implicit def TypeTestAPI(typeTest: TypeTest): TypeTestAPI</pre></code>

### TypeTreeAPI
<pre><code class="language-scala" >final implicit def TypeTreeAPI(self: TypeTree): TypeTreeAPI</pre></code>

### Type_AndTypeAPI
<pre><code class="language-scala" >final implicit def Type_AndTypeAPI(self: AndType): Type_AndTypeAPI</pre></code>

### Type_AnnotatedTypeAPI
<pre><code class="language-scala" >final implicit def Type_AnnotatedTypeAPI(self: AnnotatedType): Type_AnnotatedTypeAPI</pre></code>

### Type_AppliedTypeAPI
<pre><code class="language-scala" >final implicit def Type_AppliedTypeAPI(self: AppliedType): Type_AppliedTypeAPI</pre></code>

### Type_ByNameTypeAPI
<pre><code class="language-scala" >final implicit def Type_ByNameTypeAPI(self: ByNameType): Type_ByNameTypeAPI</pre></code>

### Type_ConstantTypeAPI
<pre><code class="language-scala" >final implicit def Type_ConstantTypeAPI(self: ConstantType): Type_ConstantTypeAPI</pre></code>

### Type_MatchTypeAPI
<pre><code class="language-scala" >final implicit def Type_MatchTypeAPI(self: MatchType): Type_MatchTypeAPI</pre></code>

### Type_MethodTypeAPI
<pre><code class="language-scala" >final implicit def Type_MethodTypeAPI(self: MethodType): Type_MethodTypeAPI</pre></code>

### Type_OrTypeAPI
<pre><code class="language-scala" >final implicit def Type_OrTypeAPI(self: OrType): Type_OrTypeAPI</pre></code>

### Type_ParamRefAPI
<pre><code class="language-scala" >final implicit def Type_ParamRefAPI(self: ParamRef): Type_ParamRefAPI</pre></code>

### Type_PolyTypeAPI
<pre><code class="language-scala" >final implicit def Type_PolyTypeAPI(self: PolyType): Type_PolyTypeAPI</pre></code>

### Type_RecursiveThisAPI
<pre><code class="language-scala" >final implicit def Type_RecursiveThisAPI(self: RecursiveThis): Type_RecursiveThisAPI</pre></code>

### Type_RecursiveTypeAPI
<pre><code class="language-scala" >final implicit def Type_RecursiveTypeAPI(self: RecursiveType): Type_RecursiveTypeAPI</pre></code>

### Type_RefinementAPI
<pre><code class="language-scala" >final implicit def Type_RefinementAPI(self: Refinement): Type_RefinementAPI</pre></code>

### Type_SuperTypeAPI
<pre><code class="language-scala" >final implicit def Type_SuperTypeAPI(self: SuperType): Type_SuperTypeAPI</pre></code>

### Type_SymRefAPI
<pre><code class="language-scala" >final implicit def Type_SymRefAPI(self: SymRef): Type_SymRefAPI</pre></code>

### Type_TermRefAPI
<pre><code class="language-scala" >final implicit def Type_TermRefAPI(self: TermRef): Type_TermRefAPI</pre></code>

### Type_ThisTypeAPI
<pre><code class="language-scala" >final implicit def Type_ThisTypeAPI(self: ThisType): Type_ThisTypeAPI</pre></code>

### Type_TypeLambdaAPI
<pre><code class="language-scala" >final implicit def Type_TypeLambdaAPI(self: TypeLambda): Type_TypeLambdaAPI</pre></code>

### Type_TypeRefAPI
<pre><code class="language-scala" >final implicit def Type_TypeRefAPI(self: TypeRef): Type_TypeRefAPI</pre></code>

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

### typeOf
<pre><code class="language-scala" >def typeOf[T](evidence$2: <a href="../../quoted/Type.md">Type</a>[T]): Type</pre></code>

