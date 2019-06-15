scala.tasty.reflect
# trait Core

<pre><code class="language-scala" >trait Core</pre></code>
Tasty reflect abstract types
```none```
+- Tree -+- PackageClause
         +- Import
         +- Statement -+- Definition --+- PackageDef
         |             |               +- ClassDef
         |             |               +- TypeDef
         |             |               +- DefDef
         |             |               +- ValDef
         |             |
         |             +- Term --------+- Ref -+- Ident
         |                             |       +- Select
         |                             |
         |                             +- Literal
         |                             +- This
         |                             +- New
         |                             +- NamedArg
         |                             +- Apply
         |                             +- TypeApply
         |                             +- Super
         |                             +- Typed
         |                             +- Assign
         |                             +- Block
         |                             +- Lambda
         |                             +- If
         |                             +- Match
         |                             +- ImpliedMatch
         |                             +- Try
         |                             +- Return
         |                             +- Repeated
         |                             +- Inlined
         |                             +- SelectOuter
         |                             +- While
         |
         |
         +- TypeTree ----+- Inferred
         |               +- TypeIdent
         |               +- TypeSelect
         |               +- Projection
         |               +- Singleton
         |               +- Refined
         |               +- Applied
         |               +- Annotated
         |               +- MatchTypeTree
         |               +- ByName
         |               +- LambdaTypeTree
         |               +- TypeBind
         |               +- TypeBlock
         |
         +- TypeBoundsTree
         +- WildcardTypeTree
         +- CaseDef
         +- TypeCaseDef
+- Pattern --+- Value
             +- Bind
             +- Unapply
             +- Alternatives
             +- TypeTest
             +- WildcardPattern
                 +- NoPrefix
+- TypeOrBounds -+- TypeBounds
                 |
                 +- Type -------+- ConstantType
                                +- SymRef
                                +- TermRef
                                +- TypeRef
                                +- SuperType
                                +- Refinement
                                +- AppliedType
                                +- AnnotatedType
                                +- AndType
                                +- OrType
                                +- MatchType
                                +- ByNameType
                                +- ParamRef
                                +- ThisType
                                +- RecursiveThis
                                +- RecursiveType
                                +- LambdaType[ParamInfo <: TypeOrBounds] -+- MethodType
                                                                          +- PolyType
                                                                          +- TypeLambda
+- ImportSelector -+- SimpleSelector
                   +- RenameSelector
                   +- OmitSelector
+- Id
+- Signature
+- Position
+- Comment
+- Constant
+- Symbol --+- PackageDefSymbol
            |
            +- TypeSymbol -+- ClassDefSymbol
            |              +- TypeDefSymbol
            |              +- TypeBindSymbol
            |
            +- TermSymbol -+- DefDefSymbol
            |              +- ValDefSymbol
            |              +- BindSymbol
            |
            +- NoSymbol
+- Flags
```
```

## Constructors:
<pre><code class="language-scala" >Core()</pre></code>

## Concrete Type Members:
### Alternatives
<pre><code class="language-scala" >type Alternatives: Alternatives</pre></code>
Pattern representing `X | Y | ...` alternatives.


### AndType
<pre><code class="language-scala" >type AndType: AndType</pre></code>
Intersection type `T & U`


### Annotated
<pre><code class="language-scala" >type Annotated: Annotated</pre></code>
Type tree representing an annotated type


### AnnotatedType
<pre><code class="language-scala" >type AnnotatedType: AnnotatedType</pre></code>
A type with an anottation `T @foo`


### Applied
<pre><code class="language-scala" >type Applied: Applied</pre></code>
Type tree representing a type application


### AppliedType
<pre><code class="language-scala" >type AppliedType: AppliedType</pre></code>
A higher kinded type applied to some types `T[U]`


### Apply
<pre><code class="language-scala" >type Apply: Apply</pre></code>
Tree an application of arguments. It represents a single list of arguments, multiple argument lists will have nested `Apply`s


### Assign
<pre><code class="language-scala" >type Assign: Assign</pre></code>
Tree representing an assignment `x = y` in the source code


### Bind
<pre><code class="language-scala" >type Bind: Bind</pre></code>
Pattern representing a `_ @ _` binding.


### BindSymbol
<pre><code class="language-scala" >type BindSymbol: BindSymbol</pre></code>
Symbol representing a bind definition.


### Block
<pre><code class="language-scala" >type Block: Block</pre></code>
Tree representing a block `{ ... }` in the source code


### ByName
<pre><code class="language-scala" >type ByName: ByName</pre></code>
Type tree representing a by name parameter


### ByNameType
<pre><code class="language-scala" >type ByNameType: ByNameType</pre></code>
Type of a by by name parameter


### CaseDef
<pre><code class="language-scala" >type CaseDef: CaseDef</pre></code>
Branch of a pattern match or catch clause


### ClassDef
<pre><code class="language-scala" >type ClassDef: ClassDef</pre></code>
Tree representing a class definition. This includes annonymus class definitions and the class of a module object


### ClassDefSymbol
<pre><code class="language-scala" >type ClassDefSymbol: ClassDefSymbol</pre></code>
Symbol of a class definition. This includes anonymous class definitions and the class of a module object.


### Comment
<pre><code class="language-scala" >type Comment: Comment</pre></code>
Comment


### Constant
<pre><code class="language-scala" >type Constant: Constant</pre></code>
Constant value represented as the constant itself


### ConstantType
<pre><code class="language-scala" >type ConstantType: ConstantType</pre></code>
A singleton type representing a known constant value


### Context
<pre><code class="language-scala" >type Context: Context</pre></code>
Compilation context


### DefDef
<pre><code class="language-scala" >type DefDef: DefDef</pre></code>
Tree representing a method definition in the source code


### DefDefSymbol
<pre><code class="language-scala" >type DefDefSymbol: DefDefSymbol</pre></code>
Symbol representing a method definition.


### Definition
<pre><code class="language-scala" >type Definition: Definition</pre></code>
Tree representing a definition in the source code. It can be `PackageDef`, `ClassDef`, `TypeDef`, `DefDef` or `ValDef`


### Flags
<pre><code class="language-scala" >type Flags: Flags</pre></code>
FlagSet of a Symbol


### Id
<pre><code class="language-scala" >type Id: Id</pre></code>
Untyped identifier


### Ident
<pre><code class="language-scala" >type Ident: Ident</pre></code>
Tree representing a reference to definition with a given name


### If
<pre><code class="language-scala" >type If: If</pre></code>
Tree representing an if/then/else `if (...) ... else ...` in the source code


### ImpliedMatch
<pre><code class="language-scala" >type ImpliedMatch: ImpliedMatch</pre></code>
Tree representing a pattern match `delegate match { ... }` in the source code


### Import
<pre><code class="language-scala" >type Import: Import</pre></code>
Tree representing an import in the source code


### ImportSelector
<pre><code class="language-scala" >type ImportSelector: ImportSelector</pre></code>
Import selectors:
 * SimpleSelector: `.bar` in `import foo.bar`
 * RenameSelector: `.{bar => baz}` in `import foo.{bar => baz}`
 * OmitSelector: `.{bar => _}` in `import foo.{bar => _}`


### Inferred
<pre><code class="language-scala" >type Inferred: Inferred</pre></code>
Type tree representing an inferred type


### Inlined
<pre><code class="language-scala" >type Inlined: Inlined</pre></code>
Tree representing the scope of an inlined tree


### Lambda
<pre><code class="language-scala" >type Lambda: Lambda</pre></code>
Tree representing a lambda `(...) => ...` in the source code


### LambdaType
<pre><code class="language-scala" >type LambdaType: [ParamInfo >: scala.Nothing <: scala.Any] => Core.this.kernel.LambdaType[ParamInfo]</pre></code>
Common abstraction for lambda types (MethodType, PolyType and TypeLambda).


### LambdaTypeTree
<pre><code class="language-scala" >type LambdaTypeTree: LambdaTypeTree</pre></code>
Type tree representing a lambda abstraction type


### Literal
<pre><code class="language-scala" >type Literal: Literal</pre></code>
Tree representing a literal value in the source code


### Match
<pre><code class="language-scala" >type Match: Match</pre></code>
Tree representing a pattern match `x match  { ... }` in the source code


### MatchType
<pre><code class="language-scala" >type MatchType: MatchType</pre></code>
Type match `T match { case U => ... }`


### MatchTypeTree
<pre><code class="language-scala" >type MatchTypeTree: MatchTypeTree</pre></code>
Type tree representing a type match


### MethodType
<pre><code class="language-scala" >type MethodType: MethodType</pre></code>
Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType.


### NamedArg
<pre><code class="language-scala" >type NamedArg: NamedArg</pre></code>
Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)`


### New
<pre><code class="language-scala" >type New: New</pre></code>
Tree representing `new` in the source code


### NoPrefix
<pre><code class="language-scala" >type NoPrefix: NoPrefix</pre></code>
NoPrefix for a type selection


### NoSymbol
<pre><code class="language-scala" >type NoSymbol: NoSymbol</pre></code>
No symbol available.


### OmitSelector
<pre><code class="language-scala" >type OmitSelector: OmitSelector</pre></code>

### OrType
<pre><code class="language-scala" >type OrType: OrType</pre></code>
Union type `T | U`


### PackageClause
<pre><code class="language-scala" >type PackageClause: PackageClause</pre></code>
Tree representing a pacakage clause in the source code


### PackageDef
<pre><code class="language-scala" >type PackageDef: PackageDef</pre></code>
Tree representing a package definition. This includes definitions in all source files


### PackageDefSymbol
<pre><code class="language-scala" >type PackageDefSymbol: PackageDefSymbol</pre></code>
Symbol of a package definition


### ParamRef
<pre><code class="language-scala" >type ParamRef: ParamRef</pre></code>
Type of a parameter reference


### Pattern
<pre><code class="language-scala" >type Pattern: Pattern</pre></code>
Pattern tree of the pattern part of a CaseDef


### PolyType
<pre><code class="language-scala" >type PolyType: PolyType</pre></code>
Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType.


### Position
<pre><code class="language-scala" >type Position: Position</pre></code>
Position in a source file


### Projection
<pre><code class="language-scala" >type Projection: Projection</pre></code>
Type tree representing a selection of definition with a given name on a given type prefix


### RecursiveThis
<pre><code class="language-scala" >type RecursiveThis: RecursiveThis</pre></code>
A type that is recursively defined `this`


### RecursiveType
<pre><code class="language-scala" >type RecursiveType: RecursiveType</pre></code>
A type that is recursively defined


### Ref
<pre><code class="language-scala" >type Ref: Ref</pre></code>
Tree representing a reference to definition


### Refined
<pre><code class="language-scala" >type Refined: Refined</pre></code>
Type tree representing a type refinement


### Refinement
<pre><code class="language-scala" >type Refinement: Refinement</pre></code>
A type with a type refinement `T { type U }`


### RenameSelector
<pre><code class="language-scala" >type RenameSelector: RenameSelector</pre></code>

### Repeated
<pre><code class="language-scala" >type Repeated: Repeated</pre></code>
Tree representing a variable argument list in the source code


### Return
<pre><code class="language-scala" >type Return: Return</pre></code>
Tree representing a `return` in the source code


### Select
<pre><code class="language-scala" >type Select: Select</pre></code>
Tree representing a selection of definition with a given name on a given prefix


### SelectOuter
<pre><code class="language-scala" >type SelectOuter: SelectOuter</pre></code>
Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees


### Settings
<pre><code class="language-scala" >type Settings: Settings</pre></code>
Settings


### Signature
<pre><code class="language-scala" >type Signature: Signature</pre></code>
JVM signature of a method


### SimpleSelector
<pre><code class="language-scala" >type SimpleSelector: SimpleSelector</pre></code>

### Singleton
<pre><code class="language-scala" >type Singleton: Singleton</pre></code>
Type tree representing a singleton type


### SourceFile
<pre><code class="language-scala" >type SourceFile: SourceFile</pre></code>
Scala source file


### Statement
<pre><code class="language-scala" >type Statement: Statement</pre></code>
Tree representing a statement in the source code


### Super
<pre><code class="language-scala" >type Super: Super</pre></code>
Tree representing `super` in the source code


### SuperType
<pre><code class="language-scala" >type SuperType: SuperType</pre></code>
Type of a `super` refernce


### SymRef
<pre><code class="language-scala" >type SymRef: SymRef</pre></code>
Type of a reference to a symbol


### Symbol
<pre><code class="language-scala" >type Symbol: Symbol</pre></code>
Symbol of a definition.
Then can be compared with == to know if the definition is the same.


### Term
<pre><code class="language-scala" >type Term: Term</pre></code>
Tree representing an expression in the source code


### TermRef
<pre><code class="language-scala" >type TermRef: TermRef</pre></code>
Type of a reference to a term


### TermSymbol
<pre><code class="language-scala" >type TermSymbol: TermSymbol</pre></code>
Symbol representing a term definition.


### This
<pre><code class="language-scala" >type This: This</pre></code>
Tree representing `this` in the source code


### ThisType
<pre><code class="language-scala" >type ThisType: ThisType</pre></code>
Type of `this`


### Tree
<pre><code class="language-scala" >type Tree: Tree</pre></code>
Tree representing code written in the source


### Try
<pre><code class="language-scala" >type Try: Try</pre></code>
Tree representing a try catch `try x catch { ... } finally { ... }` in the source code


### Type
<pre><code class="language-scala" >type Type: Type</pre></code>
A type


### TypeApply
<pre><code class="language-scala" >type TypeApply: TypeApply</pre></code>
Tree an application of type arguments


### TypeBind
<pre><code class="language-scala" >type TypeBind: TypeBind</pre></code>
Type tree representing a type binding


### TypeBindSymbol
<pre><code class="language-scala" >type TypeBindSymbol: TypeBindSymbol</pre></code>
Symbol representing a type bind definition.


### TypeBlock
<pre><code class="language-scala" >type TypeBlock: TypeBlock</pre></code>
Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }`


### TypeBounds
<pre><code class="language-scala" >type TypeBounds: TypeBounds</pre></code>
Type bounds


### TypeBoundsTree
<pre><code class="language-scala" >type TypeBoundsTree: TypeBoundsTree</pre></code>
Type tree representing a type bound written in the source


### TypeCaseDef
<pre><code class="language-scala" >type TypeCaseDef: TypeCaseDef</pre></code>
Branch of a type pattern match


### TypeDef
<pre><code class="language-scala" >type TypeDef: TypeDef</pre></code>
Tree representing a type (paramter or member) definition in the source code


### TypeDefSymbol
<pre><code class="language-scala" >type TypeDefSymbol: TypeDefSymbol</pre></code>
Symbol of a type (parameter or member) definition.


### TypeIdent
<pre><code class="language-scala" >type TypeIdent: TypeIdent</pre></code>
Type tree representing a reference to definition with a given name


### TypeLambda
<pre><code class="language-scala" >type TypeLambda: TypeLambda</pre></code>
Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda.


### TypeOrBounds
<pre><code class="language-scala" >type TypeOrBounds: TypeOrBounds</pre></code>
Type or bounds


### TypeRef
<pre><code class="language-scala" >type TypeRef: TypeRef</pre></code>
Type of a reference to a type


### TypeSelect
<pre><code class="language-scala" >type TypeSelect: TypeSelect</pre></code>
Type tree representing a selection of definition with a given name on a given term prefix


### TypeSymbol
<pre><code class="language-scala" >type TypeSymbol: TypeSymbol</pre></code>
Symbol representing a type definition.


### TypeTest
<pre><code class="language-scala" >type TypeTest: TypeTest</pre></code>
Pattern representing a `x: Y` type test.


### TypeTree
<pre><code class="language-scala" >type TypeTree: TypeTree</pre></code>
Type tree representing a type written in the source


### Typed
<pre><code class="language-scala" >type Typed: Typed</pre></code>
Tree representing a type ascription `x: T` in the source code


### Unapply
<pre><code class="language-scala" >type Unapply: Unapply</pre></code>
Pattern representing a `Xyz(...)` unapply.


### ValDef
<pre><code class="language-scala" >type ValDef: ValDef</pre></code>
Tree representing a value definition in the source code This inclues `val`, `lazy val`, `var`, `object` and parameter defintions.


### ValDefSymbol
<pre><code class="language-scala" >type ValDefSymbol: ValDefSymbol</pre></code>
Symbol representing a value definition. This includes `val`, `lazy val`, `var`, `object` and parameter definitions.


### Value
<pre><code class="language-scala" >type Value: Value</pre></code>
Pattern representing a value. This includes `1`, ```x``` and `_`


### While
<pre><code class="language-scala" >type While: While</pre></code>
Tree representing a while loop


### WildcardPattern
<pre><code class="language-scala" >type WildcardPattern: WildcardPattern</pre></code>
Pattern representing a `_` pattern


### WildcardTypeTree
<pre><code class="language-scala" >type WildcardTypeTree: WildcardTypeTree</pre></code>
Type tree representing wildcard type bounds written in the source.
The wildcard type `_` (for example in in `List[_]`) will be a type tree that
represents a type but has `TypeBound`a inside.


## Concrete Value Members:
### kernel
<pre><code class="language-scala" >val kernel: Kernel</pre></code>

