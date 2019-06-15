scala.tasty.reflect
# trait TreeOps

<pre><code class="language-scala" >trait TreeOps extends Core</pre></code>
## Known subclasses:
<a href="./TreeOps/TypeCaseDef$.md">TypeCaseDef</a>, <a href="./TreeOps/IsTypeCaseDef$.md">IsTypeCaseDef</a>, <a href="./TreeOps/TypeCaseDefAPI.md">TypeCaseDefAPI</a>, <a href="./TreeOps/CaseDef$.md">CaseDef</a>, <a href="./TreeOps/IsCaseDef$.md">IsCaseDef</a>, <a href="./TreeOps/CaseDefAPI.md">CaseDefAPI</a>, <a href="./TreeOps/WildcardTypeTree$.md">WildcardTypeTree</a>, <a href="./TreeOps/IsWildcardTypeTree$.md">IsWildcardTypeTree</a>, <a href="./TreeOps/WildcardTypeTreeAPI.md">WildcardTypeTreeAPI</a>, <a href="./TreeOps/TypeBoundsTree$.md">TypeBoundsTree</a>, <a href="./TreeOps/IsTypeBoundsTree$.md">IsTypeBoundsTree</a>, <a href="./TreeOps/TypeBoundsTreeAPI.md">TypeBoundsTreeAPI</a>, <a href="./TreeOps/TypeBlockAPI.md">TypeBlockAPI</a>, <a href="./TreeOps/TypeBlock$.md">TypeBlock</a>, <a href="./TreeOps/IsTypeBlock$.md">IsTypeBlock</a>, <a href="./TreeOps/TypeBindAPI.md">TypeBindAPI</a>, <a href="./TreeOps/TypeBind$.md">TypeBind</a>, <a href="./TreeOps/IsTypeBind$.md">IsTypeBind</a>, <a href="./TreeOps/LambdaTypeTreeAPI.md">LambdaTypeTreeAPI</a>, <a href="./TreeOps/LambdaTypeTree$.md">LambdaTypeTree</a>, <a href="./TreeOps/IsLambdaTypeTree$.md">IsLambdaTypeTree</a>, <a href="./TreeOps/ByNameAPI.md">ByNameAPI</a>, <a href="./TreeOps/ByName$.md">ByName</a>, <a href="./TreeOps/IsByName$.md">IsByName</a>, <a href="./TreeOps/MatchTypeTreeAPI.md">MatchTypeTreeAPI</a>, <a href="./TreeOps/MatchTypeTree$.md">MatchTypeTree</a>, <a href="./TreeOps/IsMatchTypeTree$.md">IsMatchTypeTree</a>, <a href="./TreeOps/AnnotatedAPI.md">AnnotatedAPI</a>, <a href="./TreeOps/Annotated$.md">Annotated</a>, <a href="./TreeOps/IsAnnotated$.md">IsAnnotated</a>, <a href="./TreeOps/AppliedAPI.md">AppliedAPI</a>, <a href="./TreeOps/Applied$.md">Applied</a>, <a href="./TreeOps/IsApplied$.md">IsApplied</a>, <a href="./TreeOps/RefinedAPI.md">RefinedAPI</a>, <a href="./TreeOps/Refined$.md">Refined</a>, <a href="./TreeOps/IsRefined$.md">IsRefined</a>, <a href="./TreeOps/SingletonAPI.md">SingletonAPI</a>, <a href="./TreeOps/Singleton$.md">Singleton</a>, <a href="./TreeOps/IsSingleton$.md">IsSingleton</a>, <a href="./TreeOps/ProjectionAPI.md">ProjectionAPI</a>, <a href="./TreeOps/Projection$.md">Projection</a>, <a href="./TreeOps/IsProjection$.md">IsProjection</a>, <a href="./TreeOps/TypeSelectAPI.md">TypeSelectAPI</a>, <a href="./TreeOps/TypeSelect$.md">TypeSelect</a>, <a href="./TreeOps/IsTypeSelect$.md">IsTypeSelect</a>, <a href="./TreeOps/TypeIdent$.md">TypeIdent</a>, <a href="./TreeOps/TypeIdentAPI.md">TypeIdentAPI</a>, <a href="./TreeOps/IsTypeIdent$.md">IsTypeIdent</a>, <a href="./TreeOps/Inferred$.md">Inferred</a>, <a href="./TreeOps/IsInferred$.md">IsInferred</a>, <a href="./TreeOps/IsTypeTree$.md">IsTypeTree</a>, <a href="./TreeOps/TypeTreeAPI.md">TypeTreeAPI</a>, <a href="./TreeOps/WhileAPI.md">WhileAPI</a>, <a href="./TreeOps/While$.md">While</a>, <a href="./TreeOps/IsWhile$.md">IsWhile</a>, <a href="./TreeOps/SelectOuterAPI.md">SelectOuterAPI</a>, <a href="./TreeOps/SelectOuter$.md">SelectOuter</a>, <a href="./TreeOps/IsSelectOuter$.md">IsSelectOuter</a>, <a href="./TreeOps/InlinedAPI.md">InlinedAPI</a>, <a href="./TreeOps/Inlined$.md">Inlined</a>, <a href="./TreeOps/IsInlined$.md">IsInlined</a>, <a href="./TreeOps/RepeatedAPI.md">RepeatedAPI</a>, <a href="./TreeOps/Repeated$.md">Repeated</a>, <a href="./TreeOps/IsRepeated$.md">IsRepeated</a>, <a href="./TreeOps/ReturnAPI.md">ReturnAPI</a>, <a href="./TreeOps/Return$.md">Return</a>, <a href="./TreeOps/IsReturn$.md">IsReturn</a>, <a href="./TreeOps/TryAPI.md">TryAPI</a>, <a href="./TreeOps/Try$.md">Try</a>, <a href="./TreeOps/IsTry$.md">IsTry</a>, <a href="./TreeOps/ImplicitMatchAPI.md">ImplicitMatchAPI</a>, <a href="./TreeOps/ImpliedMatch$.md">ImpliedMatch</a>, <a href="./TreeOps/IsImplicitMatch$.md">IsImplicitMatch</a>, <a href="./TreeOps/MatchAPI.md">MatchAPI</a>, <a href="./TreeOps/Match$.md">Match</a>, <a href="./TreeOps/IsMatch$.md">IsMatch</a>, <a href="./TreeOps/IfAPI.md">IfAPI</a>, <a href="./TreeOps/If$.md">If</a>, <a href="./TreeOps/IsIf$.md">IsIf</a>, <a href="./TreeOps/LambdaAPI.md">LambdaAPI</a>, <a href="./TreeOps/Lambda$.md">Lambda</a>, <a href="./TreeOps/IsLambda$.md">IsLambda</a>, <a href="./TreeOps/BlockAPI.md">BlockAPI</a>, <a href="./TreeOps/Block$.md">Block</a>, <a href="./TreeOps/IsBlock$.md">IsBlock</a>, <a href="./TreeOps/AssignAPI.md">AssignAPI</a>, <a href="./TreeOps/Assign$.md">Assign</a>, <a href="./TreeOps/IsAssign$.md">IsAssign</a>, <a href="./TreeOps/TypedAPI.md">TypedAPI</a>, <a href="./TreeOps/Typed$.md">Typed</a>, <a href="./TreeOps/IsTyped$.md">IsTyped</a>, <a href="./TreeOps/SuperAPI.md">SuperAPI</a>, <a href="./TreeOps/Super$.md">Super</a>, <a href="./TreeOps/IsSuper$.md">IsSuper</a>, <a href="./TreeOps/TypeApplyAPI.md">TypeApplyAPI</a>, <a href="./TreeOps/TypeApply$.md">TypeApply</a>, <a href="./TreeOps/IsTypeApply$.md">IsTypeApply</a>, <a href="./TreeOps/ApplyAPI.md">ApplyAPI</a>, <a href="./TreeOps/Apply$.md">Apply</a>, <a href="./TreeOps/IsApply$.md">IsApply</a>, <a href="./TreeOps/NamedArgAPI.md">NamedArgAPI</a>, <a href="./TreeOps/NamedArg$.md">NamedArg</a>, <a href="./TreeOps/IsNamedArg$.md">IsNamedArg</a>, <a href="./TreeOps/NewAPI.md">NewAPI</a>, <a href="./TreeOps/New$.md">New</a>, <a href="./TreeOps/IsNew$.md">IsNew</a>, <a href="./TreeOps/ThisAPI.md">ThisAPI</a>, <a href="./TreeOps/This$.md">This</a>, <a href="./TreeOps/IsThis$.md">IsThis</a>, <a href="./TreeOps/LiteralAPI.md">LiteralAPI</a>, <a href="./TreeOps/Literal$.md">Literal</a>, <a href="./TreeOps/IsLiteral$.md">IsLiteral</a>, <a href="./TreeOps/SelectAPI.md">SelectAPI</a>, <a href="./TreeOps/Select$.md">Select</a>, <a href="./TreeOps/IsSelect$.md">IsSelect</a>, <a href="./TreeOps/Ident$.md">Ident</a>, <a href="./TreeOps/IdentAPI.md">IdentAPI</a>, <a href="./TreeOps/IsIdent$.md">IsIdent</a>, <a href="./TreeOps/Ref$.md">Ref</a>, <a href="./TreeOps/IsRef$.md">IsRef</a>, <a href="./TreeOps/IsTerm$.md">IsTerm</a>, <a href="./TreeOps/TermAPI.md">TermAPI</a>, <a href="./TreeOps/PackageDef$.md">PackageDef</a>, <a href="./TreeOps/PackageDefAPI.md">PackageDefAPI</a>, <a href="./TreeOps/IsPackageDef$.md">IsPackageDef</a>, <a href="./TreeOps/TypeDefAPI.md">TypeDefAPI</a>, <a href="./TreeOps/TypeDef$.md">TypeDef</a>, <a href="./TreeOps/IsTypeDef$.md">IsTypeDef</a>, <a href="./TreeOps/ValDefAPI.md">ValDefAPI</a>, <a href="./TreeOps/ValDef$.md">ValDef</a>, <a href="./TreeOps/IsValDef$.md">IsValDef</a>, <a href="./TreeOps/DefDefAPI.md">DefDefAPI</a>, <a href="./TreeOps/DefDef$.md">DefDef</a>, <a href="./TreeOps/IsDefDef$.md">IsDefDef</a>, <a href="./TreeOps/ClassDefAPI.md">ClassDefAPI</a>, <a href="./TreeOps/ClassDef$.md">ClassDef</a>, <a href="./TreeOps/IsClassDef$.md">IsClassDef</a>, <a href="./TreeOps/DefinitionAPI.md">DefinitionAPI</a>, <a href="./TreeOps/IsDefinition$.md">IsDefinition</a>, <a href="./TreeOps/IsStatement$.md">IsStatement</a>, <a href="./TreeOps/ImportAPI.md">ImportAPI</a>, <a href="./TreeOps/Import$.md">Import</a>, <a href="./TreeOps/IsImport$.md">IsImport</a>, <a href="./TreeOps/PackageClauseAPI.md">PackageClauseAPI</a>, <a href="./TreeOps/PackageClause$.md">PackageClause</a>, <a href="./TreeOps/IsPackageClause$.md">IsPackageClause</a>, <a href="./TreeOps/TreeAPI.md">TreeAPI</a>
## Constructors:
<pre><code class="language-scala" >TreeOps()</pre></code>

## Concrete Type Members:
### Annotated
<pre><code class="language-scala" >final object <a href="./TreeOps/Annotated$.md">Annotated</a></pre></code>
### Applied
<pre><code class="language-scala" >final object <a href="./TreeOps/Applied$.md">Applied</a></pre></code>
### Apply
<pre><code class="language-scala" >final object <a href="./TreeOps/Apply$.md">Apply</a></pre></code>
Scala parameter application

### Assign
<pre><code class="language-scala" >final object <a href="./TreeOps/Assign$.md">Assign</a></pre></code>
Scala assign `x = y`

### Block
<pre><code class="language-scala" >final object <a href="./TreeOps/Block$.md">Block</a></pre></code>
Scala code block `{ stat0; ...; statN; expr }` term

### ByName
<pre><code class="language-scala" >final object <a href="./TreeOps/ByName$.md">ByName</a></pre></code>
### CaseDef
<pre><code class="language-scala" >final object <a href="./TreeOps/CaseDef$.md">CaseDef</a></pre></code>
### ClassDef
<pre><code class="language-scala" >final object <a href="./TreeOps/ClassDef$.md">ClassDef</a></pre></code>
### DefDef
<pre><code class="language-scala" >final object <a href="./TreeOps/DefDef$.md">DefDef</a></pre></code>
### Ident
<pre><code class="language-scala" >final object <a href="./TreeOps/Ident$.md">Ident</a></pre></code>
Scala term identifier

### If
<pre><code class="language-scala" >final object <a href="./TreeOps/If$.md">If</a></pre></code>
Scala `if`/`else` term

### ImpliedMatch
<pre><code class="language-scala" >final object <a href="./TreeOps/ImpliedMatch$.md">ImpliedMatch</a></pre></code>
Scala implicit `match` term

### Import
<pre><code class="language-scala" >final object <a href="./TreeOps/Import$.md">Import</a></pre></code>
### Inferred
<pre><code class="language-scala" >final object <a href="./TreeOps/Inferred$.md">Inferred</a></pre></code>
TypeTree containing an inferred type

### Inlined
<pre><code class="language-scala" >final object <a href="./TreeOps/Inlined$.md">Inlined</a></pre></code>
### IsAnnotated
<pre><code class="language-scala" >final object <a href="./TreeOps/IsAnnotated$.md">IsAnnotated</a></pre></code>
### IsApplied
<pre><code class="language-scala" >final object <a href="./TreeOps/IsApplied$.md">IsApplied</a></pre></code>
### IsApply
<pre><code class="language-scala" >final object <a href="./TreeOps/IsApply$.md">IsApply</a></pre></code>
### IsAssign
<pre><code class="language-scala" >final object <a href="./TreeOps/IsAssign$.md">IsAssign</a></pre></code>
### IsBlock
<pre><code class="language-scala" >final object <a href="./TreeOps/IsBlock$.md">IsBlock</a></pre></code>
### IsByName
<pre><code class="language-scala" >final object <a href="./TreeOps/IsByName$.md">IsByName</a></pre></code>
### IsCaseDef
<pre><code class="language-scala" >final object <a href="./TreeOps/IsCaseDef$.md">IsCaseDef</a></pre></code>
### IsClassDef
<pre><code class="language-scala" >final object <a href="./TreeOps/IsClassDef$.md">IsClassDef</a></pre></code>
### IsDefDef
<pre><code class="language-scala" >final object <a href="./TreeOps/IsDefDef$.md">IsDefDef</a></pre></code>
### IsDefinition
<pre><code class="language-scala" >final object <a href="./TreeOps/IsDefinition$.md">IsDefinition</a></pre></code>
### IsIdent
<pre><code class="language-scala" >final object <a href="./TreeOps/IsIdent$.md">IsIdent</a></pre></code>
### IsIf
<pre><code class="language-scala" >final object <a href="./TreeOps/IsIf$.md">IsIf</a></pre></code>
### IsImplicitMatch
<pre><code class="language-scala" >final object <a href="./TreeOps/IsImplicitMatch$.md">IsImplicitMatch</a></pre></code>
### IsImport
<pre><code class="language-scala" >final object <a href="./TreeOps/IsImport$.md">IsImport</a></pre></code>
### IsInferred
<pre><code class="language-scala" >final object <a href="./TreeOps/IsInferred$.md">IsInferred</a></pre></code>
### IsInlined
<pre><code class="language-scala" >final object <a href="./TreeOps/IsInlined$.md">IsInlined</a></pre></code>
### IsLambda
<pre><code class="language-scala" >final object <a href="./TreeOps/IsLambda$.md">IsLambda</a></pre></code>
### IsLambdaTypeTree
<pre><code class="language-scala" >final object <a href="./TreeOps/IsLambdaTypeTree$.md">IsLambdaTypeTree</a></pre></code>
### IsLiteral
<pre><code class="language-scala" >final object <a href="./TreeOps/IsLiteral$.md">IsLiteral</a></pre></code>
### IsMatch
<pre><code class="language-scala" >final object <a href="./TreeOps/IsMatch$.md">IsMatch</a></pre></code>
### IsMatchTypeTree
<pre><code class="language-scala" >final object <a href="./TreeOps/IsMatchTypeTree$.md">IsMatchTypeTree</a></pre></code>
### IsNamedArg
<pre><code class="language-scala" >final object <a href="./TreeOps/IsNamedArg$.md">IsNamedArg</a></pre></code>
### IsNew
<pre><code class="language-scala" >final object <a href="./TreeOps/IsNew$.md">IsNew</a></pre></code>
### IsPackageClause
<pre><code class="language-scala" >final object <a href="./TreeOps/IsPackageClause$.md">IsPackageClause</a></pre></code>
### IsPackageDef
<pre><code class="language-scala" >final object <a href="./TreeOps/IsPackageDef$.md">IsPackageDef</a></pre></code>
### IsProjection
<pre><code class="language-scala" >final object <a href="./TreeOps/IsProjection$.md">IsProjection</a></pre></code>
### IsRef
<pre><code class="language-scala" >final object <a href="./TreeOps/IsRef$.md">IsRef</a></pre></code>
### IsRefined
<pre><code class="language-scala" >final object <a href="./TreeOps/IsRefined$.md">IsRefined</a></pre></code>
### IsRepeated
<pre><code class="language-scala" >final object <a href="./TreeOps/IsRepeated$.md">IsRepeated</a></pre></code>
### IsReturn
<pre><code class="language-scala" >final object <a href="./TreeOps/IsReturn$.md">IsReturn</a></pre></code>
### IsSelect
<pre><code class="language-scala" >final object <a href="./TreeOps/IsSelect$.md">IsSelect</a></pre></code>
### IsSelectOuter
<pre><code class="language-scala" >final object <a href="./TreeOps/IsSelectOuter$.md">IsSelectOuter</a></pre></code>
### IsSingleton
<pre><code class="language-scala" >final object <a href="./TreeOps/IsSingleton$.md">IsSingleton</a></pre></code>
### IsStatement
<pre><code class="language-scala" >final object <a href="./TreeOps/IsStatement$.md">IsStatement</a></pre></code>
### IsSuper
<pre><code class="language-scala" >final object <a href="./TreeOps/IsSuper$.md">IsSuper</a></pre></code>
### IsTerm
<pre><code class="language-scala" >final object <a href="./TreeOps/IsTerm$.md">IsTerm</a></pre></code>
### IsThis
<pre><code class="language-scala" >final object <a href="./TreeOps/IsThis$.md">IsThis</a></pre></code>
### IsTry
<pre><code class="language-scala" >final object <a href="./TreeOps/IsTry$.md">IsTry</a></pre></code>
### IsTypeApply
<pre><code class="language-scala" >final object <a href="./TreeOps/IsTypeApply$.md">IsTypeApply</a></pre></code>
### IsTypeBind
<pre><code class="language-scala" >final object <a href="./TreeOps/IsTypeBind$.md">IsTypeBind</a></pre></code>
### IsTypeBlock
<pre><code class="language-scala" >final object <a href="./TreeOps/IsTypeBlock$.md">IsTypeBlock</a></pre></code>
### IsTypeBoundsTree
<pre><code class="language-scala" >final object <a href="./TreeOps/IsTypeBoundsTree$.md">IsTypeBoundsTree</a></pre></code>
### IsTypeCaseDef
<pre><code class="language-scala" >final object <a href="./TreeOps/IsTypeCaseDef$.md">IsTypeCaseDef</a></pre></code>
### IsTypeDef
<pre><code class="language-scala" >final object <a href="./TreeOps/IsTypeDef$.md">IsTypeDef</a></pre></code>
### IsTypeIdent
<pre><code class="language-scala" >final object <a href="./TreeOps/IsTypeIdent$.md">IsTypeIdent</a></pre></code>
### IsTypeSelect
<pre><code class="language-scala" >final object <a href="./TreeOps/IsTypeSelect$.md">IsTypeSelect</a></pre></code>
### IsTypeTree
<pre><code class="language-scala" >final object <a href="./TreeOps/IsTypeTree$.md">IsTypeTree</a></pre></code>
### IsTyped
<pre><code class="language-scala" >final object <a href="./TreeOps/IsTyped$.md">IsTyped</a></pre></code>
### IsValDef
<pre><code class="language-scala" >final object <a href="./TreeOps/IsValDef$.md">IsValDef</a></pre></code>
### IsWhile
<pre><code class="language-scala" >final object <a href="./TreeOps/IsWhile$.md">IsWhile</a></pre></code>
### IsWildcardTypeTree
<pre><code class="language-scala" >final object <a href="./TreeOps/IsWildcardTypeTree$.md">IsWildcardTypeTree</a></pre></code>
### Lambda
<pre><code class="language-scala" >final object <a href="./TreeOps/Lambda$.md">Lambda</a></pre></code>
### LambdaTypeTree
<pre><code class="language-scala" >final object <a href="./TreeOps/LambdaTypeTree$.md">LambdaTypeTree</a></pre></code>
### Literal
<pre><code class="language-scala" >final object <a href="./TreeOps/Literal$.md">Literal</a></pre></code>
Scala literal constant

### Match
<pre><code class="language-scala" >final object <a href="./TreeOps/Match$.md">Match</a></pre></code>
Scala `match` term

### MatchTypeTree
<pre><code class="language-scala" >final object <a href="./TreeOps/MatchTypeTree$.md">MatchTypeTree</a></pre></code>
### NamedArg
<pre><code class="language-scala" >final object <a href="./TreeOps/NamedArg$.md">NamedArg</a></pre></code>
Scala named argument `x = y` in argument position

### New
<pre><code class="language-scala" >final object <a href="./TreeOps/New$.md">New</a></pre></code>
Scala `new`

### PackageClause
<pre><code class="language-scala" >final object <a href="./TreeOps/PackageClause$.md">PackageClause</a></pre></code>
### PackageDef
<pre><code class="language-scala" >final object <a href="./TreeOps/PackageDef$.md">PackageDef</a></pre></code>
### Projection
<pre><code class="language-scala" >final object <a href="./TreeOps/Projection$.md">Projection</a></pre></code>
### Ref
<pre><code class="language-scala" >final object <a href="./TreeOps/Ref$.md">Ref</a></pre></code>
### Refined
<pre><code class="language-scala" >final object <a href="./TreeOps/Refined$.md">Refined</a></pre></code>
### Repeated
<pre><code class="language-scala" >final object <a href="./TreeOps/Repeated$.md">Repeated</a></pre></code>
### Return
<pre><code class="language-scala" >final object <a href="./TreeOps/Return$.md">Return</a></pre></code>
Scala local `return`

### Select
<pre><code class="language-scala" >final object <a href="./TreeOps/Select$.md">Select</a></pre></code>
Scala term selection

### SelectOuter
<pre><code class="language-scala" >final object <a href="./TreeOps/SelectOuter$.md">SelectOuter</a></pre></code>
### Singleton
<pre><code class="language-scala" >final object <a href="./TreeOps/Singleton$.md">Singleton</a></pre></code>
### Super
<pre><code class="language-scala" >final object <a href="./TreeOps/Super$.md">Super</a></pre></code>
Scala `x.super` or `x.super[id]`

### This
<pre><code class="language-scala" >final object <a href="./TreeOps/This$.md">This</a></pre></code>
Scala `this` or `this[id]`

### Try
<pre><code class="language-scala" >final object <a href="./TreeOps/Try$.md">Try</a></pre></code>
Scala `try`/`catch`/`finally` term

### TypeApply
<pre><code class="language-scala" >final object <a href="./TreeOps/TypeApply$.md">TypeApply</a></pre></code>
Scala type parameter application

### TypeBind
<pre><code class="language-scala" >final object <a href="./TreeOps/TypeBind$.md">TypeBind</a></pre></code>
### TypeBlock
<pre><code class="language-scala" >final object <a href="./TreeOps/TypeBlock$.md">TypeBlock</a></pre></code>
### TypeBoundsTree
<pre><code class="language-scala" >final object <a href="./TreeOps/TypeBoundsTree$.md">TypeBoundsTree</a></pre></code>
### TypeCaseDef
<pre><code class="language-scala" >final object <a href="./TreeOps/TypeCaseDef$.md">TypeCaseDef</a></pre></code>
### TypeDef
<pre><code class="language-scala" >final object <a href="./TreeOps/TypeDef$.md">TypeDef</a></pre></code>
### TypeIdent
<pre><code class="language-scala" >final object <a href="./TreeOps/TypeIdent$.md">TypeIdent</a></pre></code>
### TypeSelect
<pre><code class="language-scala" >final object <a href="./TreeOps/TypeSelect$.md">TypeSelect</a></pre></code>
### Typed
<pre><code class="language-scala" >final object <a href="./TreeOps/Typed$.md">Typed</a></pre></code>
Scala ascription `x: T`

### ValDef
<pre><code class="language-scala" >final object <a href="./TreeOps/ValDef$.md">ValDef</a></pre></code>
### While
<pre><code class="language-scala" >final object <a href="./TreeOps/While$.md">While</a></pre></code>
### WildcardTypeTree
<pre><code class="language-scala" >final object <a href="./TreeOps/WildcardTypeTree$.md">WildcardTypeTree</a></pre></code>
TypeBoundsTree containing wildcard type bounds

### AnnotatedAPI
<pre><code class="language-scala" >class <a href="./TreeOps/AnnotatedAPI.md">AnnotatedAPI</a></pre></code>
### AppliedAPI
<pre><code class="language-scala" >class <a href="./TreeOps/AppliedAPI.md">AppliedAPI</a></pre></code>
### ApplyAPI
<pre><code class="language-scala" >class <a href="./TreeOps/ApplyAPI.md">ApplyAPI</a></pre></code>
### AssignAPI
<pre><code class="language-scala" >class <a href="./TreeOps/AssignAPI.md">AssignAPI</a></pre></code>
### BlockAPI
<pre><code class="language-scala" >class <a href="./TreeOps/BlockAPI.md">BlockAPI</a></pre></code>
### ByNameAPI
<pre><code class="language-scala" >class <a href="./TreeOps/ByNameAPI.md">ByNameAPI</a></pre></code>
### CaseDefAPI
<pre><code class="language-scala" >class <a href="./TreeOps/CaseDefAPI.md">CaseDefAPI</a></pre></code>
### ClassDefAPI
<pre><code class="language-scala" >class <a href="./TreeOps/ClassDefAPI.md">ClassDefAPI</a></pre></code>
### DefDefAPI
<pre><code class="language-scala" >class <a href="./TreeOps/DefDefAPI.md">DefDefAPI</a></pre></code>
### DefinitionAPI
<pre><code class="language-scala" >class <a href="./TreeOps/DefinitionAPI.md">DefinitionAPI</a></pre></code>
### IdentAPI
<pre><code class="language-scala" >class <a href="./TreeOps/IdentAPI.md">IdentAPI</a></pre></code>
### IfAPI
<pre><code class="language-scala" >class <a href="./TreeOps/IfAPI.md">IfAPI</a></pre></code>
### ImplicitMatchAPI
<pre><code class="language-scala" >class <a href="./TreeOps/ImplicitMatchAPI.md">ImplicitMatchAPI</a></pre></code>
### ImportAPI
<pre><code class="language-scala" >class <a href="./TreeOps/ImportAPI.md">ImportAPI</a></pre></code>
### InlinedAPI
<pre><code class="language-scala" >class <a href="./TreeOps/InlinedAPI.md">InlinedAPI</a></pre></code>
### LambdaAPI
<pre><code class="language-scala" >class <a href="./TreeOps/LambdaAPI.md">LambdaAPI</a></pre></code>
### LambdaTypeTreeAPI
<pre><code class="language-scala" >class <a href="./TreeOps/LambdaTypeTreeAPI.md">LambdaTypeTreeAPI</a></pre></code>
### LiteralAPI
<pre><code class="language-scala" >class <a href="./TreeOps/LiteralAPI.md">LiteralAPI</a></pre></code>
### MatchAPI
<pre><code class="language-scala" >class <a href="./TreeOps/MatchAPI.md">MatchAPI</a></pre></code>
### MatchTypeTreeAPI
<pre><code class="language-scala" >class <a href="./TreeOps/MatchTypeTreeAPI.md">MatchTypeTreeAPI</a></pre></code>
### NamedArgAPI
<pre><code class="language-scala" >class <a href="./TreeOps/NamedArgAPI.md">NamedArgAPI</a></pre></code>
### NewAPI
<pre><code class="language-scala" >class <a href="./TreeOps/NewAPI.md">NewAPI</a></pre></code>
### PackageClauseAPI
<pre><code class="language-scala" >class <a href="./TreeOps/PackageClauseAPI.md">PackageClauseAPI</a></pre></code>
### PackageDefAPI
<pre><code class="language-scala" >class <a href="./TreeOps/PackageDefAPI.md">PackageDefAPI</a></pre></code>
### ProjectionAPI
<pre><code class="language-scala" >class <a href="./TreeOps/ProjectionAPI.md">ProjectionAPI</a></pre></code>
### RefinedAPI
<pre><code class="language-scala" >class <a href="./TreeOps/RefinedAPI.md">RefinedAPI</a></pre></code>
### RepeatedAPI
<pre><code class="language-scala" >class <a href="./TreeOps/RepeatedAPI.md">RepeatedAPI</a></pre></code>
### ReturnAPI
<pre><code class="language-scala" >class <a href="./TreeOps/ReturnAPI.md">ReturnAPI</a></pre></code>
### SelectAPI
<pre><code class="language-scala" >class <a href="./TreeOps/SelectAPI.md">SelectAPI</a></pre></code>
### SelectOuterAPI
<pre><code class="language-scala" >class <a href="./TreeOps/SelectOuterAPI.md">SelectOuterAPI</a></pre></code>
### SingletonAPI
<pre><code class="language-scala" >class <a href="./TreeOps/SingletonAPI.md">SingletonAPI</a></pre></code>
### SuperAPI
<pre><code class="language-scala" >class <a href="./TreeOps/SuperAPI.md">SuperAPI</a></pre></code>
### TermAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TermAPI.md">TermAPI</a></pre></code>
### ThisAPI
<pre><code class="language-scala" >class <a href="./TreeOps/ThisAPI.md">ThisAPI</a></pre></code>
### TreeAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TreeAPI.md">TreeAPI</a></pre></code>
### TryAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TryAPI.md">TryAPI</a></pre></code>
### TypeApplyAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TypeApplyAPI.md">TypeApplyAPI</a></pre></code>
### TypeBindAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TypeBindAPI.md">TypeBindAPI</a></pre></code>
### TypeBlockAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TypeBlockAPI.md">TypeBlockAPI</a></pre></code>
### TypeBoundsTreeAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TypeBoundsTreeAPI.md">TypeBoundsTreeAPI</a></pre></code>
### TypeCaseDefAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TypeCaseDefAPI.md">TypeCaseDefAPI</a></pre></code>
### TypeDefAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TypeDefAPI.md">TypeDefAPI</a></pre></code>
### TypeIdentAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TypeIdentAPI.md">TypeIdentAPI</a></pre></code>
### TypeSelectAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TypeSelectAPI.md">TypeSelectAPI</a></pre></code>
### TypeTreeAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TypeTreeAPI.md">TypeTreeAPI</a></pre></code>
### TypedAPI
<pre><code class="language-scala" >class <a href="./TreeOps/TypedAPI.md">TypedAPI</a></pre></code>
### ValDefAPI
<pre><code class="language-scala" >class <a href="./TreeOps/ValDefAPI.md">ValDefAPI</a></pre></code>
### WhileAPI
<pre><code class="language-scala" >class <a href="./TreeOps/WhileAPI.md">WhileAPI</a></pre></code>
### WildcardTypeTreeAPI
<pre><code class="language-scala" >class <a href="./TreeOps/WildcardTypeTreeAPI.md">WildcardTypeTreeAPI</a></pre></code>
## Concrete Value Members:
### AnnotatedAPI
<pre><code class="language-scala" >final implicit def AnnotatedAPI(self: Annotated): AnnotatedAPI</pre></code>

### AppliedAPI
<pre><code class="language-scala" >final implicit def AppliedAPI(self: Applied): AppliedAPI</pre></code>

### ApplyAPI
<pre><code class="language-scala" >final implicit def ApplyAPI(self: Apply): ApplyAPI</pre></code>

### AssignAPI
<pre><code class="language-scala" >final implicit def AssignAPI(self: Assign): AssignAPI</pre></code>

### BlockAPI
<pre><code class="language-scala" >final implicit def BlockAPI(self: Block): BlockAPI</pre></code>

### ByNameAPI
<pre><code class="language-scala" >final implicit def ByNameAPI(self: ByName): ByNameAPI</pre></code>

### CaseDefAPI
<pre><code class="language-scala" >final implicit def CaseDefAPI(caseDef: CaseDef): CaseDefAPI</pre></code>

### ClassDefAPI
<pre><code class="language-scala" >final implicit def ClassDefAPI(self: ClassDef): ClassDefAPI</pre></code>

### DefDefAPI
<pre><code class="language-scala" >final implicit def DefDefAPI(self: DefDef): DefDefAPI</pre></code>

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

### TypeBlockAPI
<pre><code class="language-scala" >final implicit def TypeBlockAPI(self: TypeBlock): TypeBlockAPI</pre></code>

### TypeBoundsTreeAPI
<pre><code class="language-scala" >final implicit def TypeBoundsTreeAPI(self: TypeBoundsTree): TypeBoundsTreeAPI</pre></code>

### TypeCaseDefAPI
<pre><code class="language-scala" >final implicit def TypeCaseDefAPI(caseDef: TypeCaseDef): TypeCaseDefAPI</pre></code>

### TypeDefAPI
<pre><code class="language-scala" >final implicit def TypeDefAPI(self: TypeDef): TypeDefAPI</pre></code>

### TypeIdentAPI
<pre><code class="language-scala" >final implicit def TypeIdentAPI(self: TypeIdent): TypeIdentAPI</pre></code>

### TypeSelectAPI
<pre><code class="language-scala" >final implicit def TypeSelectAPI(self: TypeSelect): TypeSelectAPI</pre></code>

### TypeTreeAPI
<pre><code class="language-scala" >final implicit def TypeTreeAPI(self: TypeTree): TypeTreeAPI</pre></code>

### TypedAPI
<pre><code class="language-scala" >final implicit def TypedAPI(self: Typed): TypedAPI</pre></code>

### ValDefAPI
<pre><code class="language-scala" >final implicit def ValDefAPI(self: ValDef): ValDefAPI</pre></code>

### WhileAPI
<pre><code class="language-scala" >final implicit def WhileAPI(self: While): WhileAPI</pre></code>

### WildcardTypeTreeAPI
<pre><code class="language-scala" >final implicit def WildcardTypeTreeAPI(self: WildcardTypeTree): WildcardTypeTreeAPI</pre></code>

### Annotated
<pre><code class="language-scala" >final val Annotated: Annotated</pre></code>

### Applied
<pre><code class="language-scala" >final val Applied: Applied</pre></code>

### Apply
<pre><code class="language-scala" >final val Apply: Apply</pre></code>
Scala parameter application


### Assign
<pre><code class="language-scala" >final val Assign: Assign</pre></code>
Scala assign `x = y`


### Block
<pre><code class="language-scala" >final val Block: Block</pre></code>
Scala code block `{ stat0; ...; statN; expr }` term


### ByName
<pre><code class="language-scala" >final val ByName: ByName</pre></code>

### CaseDef
<pre><code class="language-scala" >final val CaseDef: CaseDef</pre></code>

### ClassDef
<pre><code class="language-scala" >final val ClassDef: ClassDef</pre></code>

### DefDef
<pre><code class="language-scala" >final val DefDef: DefDef</pre></code>

### Ident
<pre><code class="language-scala" >final val Ident: Ident</pre></code>
Scala term identifier


### If
<pre><code class="language-scala" >final val If: If</pre></code>
Scala `if`/`else` term


### ImpliedMatch
<pre><code class="language-scala" >final val ImpliedMatch: ImpliedMatch</pre></code>
Scala implicit `match` term


### Import
<pre><code class="language-scala" >final val Import: Import</pre></code>

### Inferred
<pre><code class="language-scala" >final val Inferred: Inferred</pre></code>
TypeTree containing an inferred type


### Inlined
<pre><code class="language-scala" >final val Inlined: Inlined</pre></code>

### IsAnnotated
<pre><code class="language-scala" >final val IsAnnotated: IsAnnotated</pre></code>

### IsApplied
<pre><code class="language-scala" >final val IsApplied: IsApplied</pre></code>

### IsApply
<pre><code class="language-scala" >final val IsApply: IsApply</pre></code>

### IsAssign
<pre><code class="language-scala" >final val IsAssign: IsAssign</pre></code>

### IsBlock
<pre><code class="language-scala" >final val IsBlock: IsBlock</pre></code>

### IsByName
<pre><code class="language-scala" >final val IsByName: IsByName</pre></code>

### IsCaseDef
<pre><code class="language-scala" >final val IsCaseDef: IsCaseDef</pre></code>

### IsClassDef
<pre><code class="language-scala" >final val IsClassDef: IsClassDef</pre></code>

### IsDefDef
<pre><code class="language-scala" >final val IsDefDef: IsDefDef</pre></code>

### IsDefinition
<pre><code class="language-scala" >final val IsDefinition: IsDefinition</pre></code>

### IsIdent
<pre><code class="language-scala" >final val IsIdent: IsIdent</pre></code>

### IsIf
<pre><code class="language-scala" >final val IsIf: IsIf</pre></code>

### IsImplicitMatch
<pre><code class="language-scala" >final val IsImplicitMatch: IsImplicitMatch</pre></code>

### IsImport
<pre><code class="language-scala" >final val IsImport: IsImport</pre></code>

### IsInferred
<pre><code class="language-scala" >final val IsInferred: IsInferred</pre></code>

### IsInlined
<pre><code class="language-scala" >final val IsInlined: IsInlined</pre></code>

### IsLambda
<pre><code class="language-scala" >final val IsLambda: IsLambda</pre></code>

### IsLambdaTypeTree
<pre><code class="language-scala" >final val IsLambdaTypeTree: IsLambdaTypeTree</pre></code>

### IsLiteral
<pre><code class="language-scala" >final val IsLiteral: IsLiteral</pre></code>

### IsMatch
<pre><code class="language-scala" >final val IsMatch: IsMatch</pre></code>

### IsMatchTypeTree
<pre><code class="language-scala" >final val IsMatchTypeTree: IsMatchTypeTree</pre></code>

### IsNamedArg
<pre><code class="language-scala" >final val IsNamedArg: IsNamedArg</pre></code>

### IsNew
<pre><code class="language-scala" >final val IsNew: IsNew</pre></code>

### IsPackageClause
<pre><code class="language-scala" >final val IsPackageClause: IsPackageClause</pre></code>

### IsPackageDef
<pre><code class="language-scala" >final val IsPackageDef: IsPackageDef</pre></code>

### IsProjection
<pre><code class="language-scala" >final val IsProjection: IsProjection</pre></code>

### IsRef
<pre><code class="language-scala" >final val IsRef: IsRef</pre></code>

### IsRefined
<pre><code class="language-scala" >final val IsRefined: IsRefined</pre></code>

### IsRepeated
<pre><code class="language-scala" >final val IsRepeated: IsRepeated</pre></code>

### IsReturn
<pre><code class="language-scala" >final val IsReturn: IsReturn</pre></code>

### IsSelect
<pre><code class="language-scala" >final val IsSelect: IsSelect</pre></code>

### IsSelectOuter
<pre><code class="language-scala" >final val IsSelectOuter: IsSelectOuter</pre></code>

### IsSingleton
<pre><code class="language-scala" >final val IsSingleton: IsSingleton</pre></code>

### IsStatement
<pre><code class="language-scala" >final val IsStatement: IsStatement</pre></code>

### IsSuper
<pre><code class="language-scala" >final val IsSuper: IsSuper</pre></code>

### IsTerm
<pre><code class="language-scala" >final val IsTerm: IsTerm</pre></code>

### IsThis
<pre><code class="language-scala" >final val IsThis: IsThis</pre></code>

### IsTry
<pre><code class="language-scala" >final val IsTry: IsTry</pre></code>

### IsTypeApply
<pre><code class="language-scala" >final val IsTypeApply: IsTypeApply</pre></code>

### IsTypeBind
<pre><code class="language-scala" >final val IsTypeBind: IsTypeBind</pre></code>

### IsTypeBlock
<pre><code class="language-scala" >final val IsTypeBlock: IsTypeBlock</pre></code>

### IsTypeBoundsTree
<pre><code class="language-scala" >final val IsTypeBoundsTree: IsTypeBoundsTree</pre></code>

### IsTypeCaseDef
<pre><code class="language-scala" >final val IsTypeCaseDef: IsTypeCaseDef</pre></code>

### IsTypeDef
<pre><code class="language-scala" >final val IsTypeDef: IsTypeDef</pre></code>

### IsTypeIdent
<pre><code class="language-scala" >final val IsTypeIdent: IsTypeIdent</pre></code>

### IsTypeSelect
<pre><code class="language-scala" >final val IsTypeSelect: IsTypeSelect</pre></code>

### IsTypeTree
<pre><code class="language-scala" >final val IsTypeTree: IsTypeTree</pre></code>

### IsTyped
<pre><code class="language-scala" >final val IsTyped: IsTyped</pre></code>

### IsValDef
<pre><code class="language-scala" >final val IsValDef: IsValDef</pre></code>

### IsWhile
<pre><code class="language-scala" >final val IsWhile: IsWhile</pre></code>

### IsWildcardTypeTree
<pre><code class="language-scala" >final val IsWildcardTypeTree: IsWildcardTypeTree</pre></code>

### Lambda
<pre><code class="language-scala" >final val Lambda: Lambda</pre></code>

### LambdaTypeTree
<pre><code class="language-scala" >final val LambdaTypeTree: LambdaTypeTree</pre></code>

### Literal
<pre><code class="language-scala" >final val Literal: Literal</pre></code>
Scala literal constant


### Match
<pre><code class="language-scala" >final val Match: Match</pre></code>
Scala `match` term


### MatchTypeTree
<pre><code class="language-scala" >final val MatchTypeTree: MatchTypeTree</pre></code>

### NamedArg
<pre><code class="language-scala" >final val NamedArg: NamedArg</pre></code>
Scala named argument `x = y` in argument position


### New
<pre><code class="language-scala" >final val New: New</pre></code>
Scala `new`


### PackageClause
<pre><code class="language-scala" >final val PackageClause: PackageClause</pre></code>

### PackageDef
<pre><code class="language-scala" >final val PackageDef: PackageDef</pre></code>

### Projection
<pre><code class="language-scala" >final val Projection: Projection</pre></code>

### Ref
<pre><code class="language-scala" >final val Ref: Ref</pre></code>

### Refined
<pre><code class="language-scala" >final val Refined: Refined</pre></code>

### Repeated
<pre><code class="language-scala" >final val Repeated: Repeated</pre></code>

### Return
<pre><code class="language-scala" >final val Return: Return</pre></code>
Scala local `return`


### Select
<pre><code class="language-scala" >final val Select: Select</pre></code>
Scala term selection


### SelectOuter
<pre><code class="language-scala" >final val SelectOuter: SelectOuter</pre></code>

### Singleton
<pre><code class="language-scala" >final val Singleton: Singleton</pre></code>

### Super
<pre><code class="language-scala" >final val Super: Super</pre></code>
Scala `x.super` or `x.super[id]`


### This
<pre><code class="language-scala" >final val This: This</pre></code>
Scala `this` or `this[id]`


### Try
<pre><code class="language-scala" >final val Try: Try</pre></code>
Scala `try`/`catch`/`finally` term


### TypeApply
<pre><code class="language-scala" >final val TypeApply: TypeApply</pre></code>
Scala type parameter application


### TypeBind
<pre><code class="language-scala" >final val TypeBind: TypeBind</pre></code>

### TypeBlock
<pre><code class="language-scala" >final val TypeBlock: TypeBlock</pre></code>

### TypeBoundsTree
<pre><code class="language-scala" >final val TypeBoundsTree: TypeBoundsTree</pre></code>

### TypeCaseDef
<pre><code class="language-scala" >final val TypeCaseDef: TypeCaseDef</pre></code>

### TypeDef
<pre><code class="language-scala" >final val TypeDef: TypeDef</pre></code>

### TypeIdent
<pre><code class="language-scala" >final val TypeIdent: TypeIdent</pre></code>

### TypeSelect
<pre><code class="language-scala" >final val TypeSelect: TypeSelect</pre></code>

### Typed
<pre><code class="language-scala" >final val Typed: Typed</pre></code>
Scala ascription `x: T`


### ValDef
<pre><code class="language-scala" >final val ValDef: ValDef</pre></code>

### While
<pre><code class="language-scala" >final val While: While</pre></code>

### WildcardTypeTree
<pre><code class="language-scala" >final val WildcardTypeTree: WildcardTypeTree</pre></code>
TypeBoundsTree containing wildcard type bounds


