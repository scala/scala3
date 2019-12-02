scala.tasty.reflect
# trait Kernel

<pre><code class="language-scala" >trait Kernel</pre></code>
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

## Concrete Type Members:
### Alternatives
<pre><code class="language-scala" >type Alternatives: Nothing <: Pattern</pre></code>
Pattern representing `X | Y | ...` alternatives.


### AndType
<pre><code class="language-scala" >type AndType: Nothing <: Type</pre></code>
Intersection type `T & U`


### Annotated
<pre><code class="language-scala" >type Annotated: Nothing <: TypeTree</pre></code>
Type tree representing an annotated type


### AnnotatedType
<pre><code class="language-scala" >type AnnotatedType: Nothing <: Type</pre></code>
A type with an anottation `T @foo`


### Applied
<pre><code class="language-scala" >type Applied: Nothing <: TypeTree</pre></code>
Type tree representing a type application


### AppliedType
<pre><code class="language-scala" >type AppliedType: Nothing <: Type</pre></code>
A higher kinded type applied to some types `T[U]`


### Apply
<pre><code class="language-scala" >type Apply: Nothing <: Term</pre></code>
Tree an application of arguments. It represents a single list of arguments, multiple argument lists will have nested `Apply`s


### Assign
<pre><code class="language-scala" >type Assign: Nothing <: Term</pre></code>
Tree representing an assignment `x = y` in the source code


### Bind
<pre><code class="language-scala" >type Bind: Nothing <: Pattern</pre></code>
Pattern representing a `_ @ _` binding.


### BindSymbol
<pre><code class="language-scala" >type BindSymbol: Nothing <: TermSymbol</pre></code>
Symbol representing a bind definition.


### Block
<pre><code class="language-scala" >type Block: Nothing <: Term</pre></code>
Tree representing a block `{ ... }` in the source code


### ByName
<pre><code class="language-scala" >type ByName: Nothing <: TypeTree</pre></code>
Type tree representing a by name parameter


### ByNameType
<pre><code class="language-scala" >type ByNameType: Nothing <: Type</pre></code>
Type of a by by name parameter


### CaseDef
<pre><code class="language-scala" >type CaseDef: Nothing <: Tree</pre></code>
Branch of a pattern match or catch clause


### ClassDef
<pre><code class="language-scala" >type ClassDef: Nothing <: Definition</pre></code>
Tree representing a class definition. This includes annonymus class definitions and the class of a module object


### ClassDefSymbol
<pre><code class="language-scala" >type ClassDefSymbol: Nothing <: TypeSymbol</pre></code>
Symbol of a class definition. This includes anonymous class definitions and the class of a module object.


### Comment
<pre><code class="language-scala" >type Comment: Nothing <: AnyRef</pre></code>
Comment


### Constant
<pre><code class="language-scala" >type Constant: Nothing <: AnyRef</pre></code>
Constant value represented as the constant itself


### ConstantType
<pre><code class="language-scala" >type ConstantType: Nothing <: Type</pre></code>
A singleton type representing a known constant value


### Context
<pre><code class="language-scala" >type Context: Nothing <: AnyRef</pre></code>
Compilation context


### DefDef
<pre><code class="language-scala" >type DefDef: Nothing <: Definition</pre></code>
Tree representing a method definition in the source code


### DefDefSymbol
<pre><code class="language-scala" >type DefDefSymbol: Nothing <: TermSymbol</pre></code>
Symbol representing a method definition.


### Definition
<pre><code class="language-scala" >type Definition: Nothing <: Statement</pre></code>
Tree representing a definition in the source code. It can be `PackageDef`, `ClassDef`, `TypeDef`, `DefDef` or `ValDef`


### Flags
<pre><code class="language-scala" >type Flags: Nothing <: Any</pre></code>
FlagSet of a Symbol


### Id
<pre><code class="language-scala" >type Id: Nothing <: AnyRef</pre></code>
Untyped identifier


### Ident
<pre><code class="language-scala" >type Ident: Nothing <: Ref</pre></code>
Tree representing a reference to definition with a given name


### If
<pre><code class="language-scala" >type If: Nothing <: Term</pre></code>
Tree representing an if/then/else `if (...) ... else ...` in the source code


### ImpliedMatch
<pre><code class="language-scala" >type ImpliedMatch: Nothing <: Term</pre></code>
Tree representing a pattern match `delegate match  { ... }` in the source code


### Import
<pre><code class="language-scala" >type Import: Nothing <: Statement</pre></code>
Tree representing an import in the source code


### ImportSelector
<pre><code class="language-scala" >type ImportSelector: Nothing <: AnyRef</pre></code>
Import selectors:
* SimpleSelector: `.bar` in `import foo.bar`
* RenameSelector: `.{bar => baz}` in `import foo.{bar => baz}`
* OmitSelector: `.{bar => _}` in `import foo.{bar => _}`


### Inferred
<pre><code class="language-scala" >type Inferred: Nothing <: TypeTree</pre></code>
Type tree representing an inferred type


### Inlined
<pre><code class="language-scala" >type Inlined: Nothing <: Term</pre></code>
Tree representing the scope of an inlined tree


### Lambda
<pre><code class="language-scala" >type Lambda: Nothing <: Term</pre></code>
Tree representing a lambda `(...) => ...` in the source code


### LambdaType
<pre><code class="language-scala" >type LambdaType: Nothing <: [ParamInfo >: scala.Nothing <: scala.Any] => Kernel.this.Type</pre></code>
Common abstraction for lambda types (MethodType, PolyType and TypeLambda).


### LambdaTypeTree
<pre><code class="language-scala" >type LambdaTypeTree: Nothing <: TypeTree</pre></code>
Type tree representing a lambda abstraction type


### Literal
<pre><code class="language-scala" >type Literal: Nothing <: Term</pre></code>
Tree representing a literal value in the source code


### Match
<pre><code class="language-scala" >type Match: Nothing <: Term</pre></code>
Tree representing a pattern match `x match  { ... }` in the source code


### MatchType
<pre><code class="language-scala" >type MatchType: Nothing <: Type</pre></code>
Type match `T match { case U => ... }`


### MatchTypeTree
<pre><code class="language-scala" >type MatchTypeTree: Nothing <: TypeTree</pre></code>
Type tree representing a type match


### MethodType
<pre><code class="language-scala" >type MethodType: Nothing <: LambdaType[Type]</pre></code>
Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType.


### NamedArg
<pre><code class="language-scala" >type NamedArg: Nothing <: Term</pre></code>
Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)`


### New
<pre><code class="language-scala" >type New: Nothing <: Term</pre></code>
Tree representing `new` in the source code


### NoPrefix
<pre><code class="language-scala" >type NoPrefix: Nothing <: TypeOrBounds</pre></code>
NoPrefix for a type selection


### NoSymbol
<pre><code class="language-scala" >type NoSymbol: Nothing <: Symbol</pre></code>
No symbol available.


### OmitSelector
<pre><code class="language-scala" >type OmitSelector: Nothing <: ImportSelector</pre></code>

### OrType
<pre><code class="language-scala" >type OrType: Nothing <: Type</pre></code>
Union type `T | U`


### PackageClause
<pre><code class="language-scala" >type PackageClause: Nothing <: Tree</pre></code>
Tree representing a pacakage clause in the source code


### PackageDef
<pre><code class="language-scala" >type PackageDef: Nothing <: Definition</pre></code>
Tree representing a package definition. This includes definitions in all source files


### PackageDefSymbol
<pre><code class="language-scala" >type PackageDefSymbol: Nothing <: Symbol</pre></code>
Symbol of a package definition


### ParamRef
<pre><code class="language-scala" >type ParamRef: Nothing <: Type</pre></code>
Type of a parameter reference


### Pattern
<pre><code class="language-scala" >type Pattern: Nothing <: AnyRef</pre></code>
Pattern tree of the pattern part of a CaseDef


### PolyType
<pre><code class="language-scala" >type PolyType: Nothing <: LambdaType[TypeBounds]</pre></code>
Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType.


### Position
<pre><code class="language-scala" >type Position: Nothing <: AnyRef</pre></code>
Position in a source file


### Projection
<pre><code class="language-scala" >type Projection: Nothing <: TypeTree</pre></code>
Type tree representing a selection of definition with a given name on a given type prefix


### RecursiveThis
<pre><code class="language-scala" >type RecursiveThis: Nothing <: Type</pre></code>
A type that is recursively defined `this`


### RecursiveType
<pre><code class="language-scala" >type RecursiveType: Nothing <: Type</pre></code>
A type that is recursively defined


### Ref
<pre><code class="language-scala" >type Ref: Nothing <: Term</pre></code>
Tree representing a reference to definition


### Refined
<pre><code class="language-scala" >type Refined: Nothing <: TypeTree</pre></code>
Type tree representing a type refinement


### Refinement
<pre><code class="language-scala" >type Refinement: Nothing <: Type</pre></code>
A type with a type refinement `T { type U }`


### RenameSelector
<pre><code class="language-scala" >type RenameSelector: Nothing <: ImportSelector</pre></code>

### Repeated
<pre><code class="language-scala" >type Repeated: Nothing <: Term</pre></code>
Tree representing a variable argument list in the source code


### Return
<pre><code class="language-scala" >type Return: Nothing <: Term</pre></code>
Tree representing a `return` in the source code


### Select
<pre><code class="language-scala" >type Select: Nothing <: Ref</pre></code>
Tree representing a selection of definition with a given name on a given prefix


### SelectOuter
<pre><code class="language-scala" >type SelectOuter: Nothing <: Term</pre></code>
Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees


### Settings
<pre><code class="language-scala" >type Settings: Nothing <: AnyRef</pre></code>
Settings


### Signature
<pre><code class="language-scala" >type Signature: Nothing <: AnyRef</pre></code>
JVM signature of a method


### SimpleSelector
<pre><code class="language-scala" >type SimpleSelector: Nothing <: ImportSelector</pre></code>

### Singleton
<pre><code class="language-scala" >type Singleton: Nothing <: TypeTree</pre></code>
Type tree representing a singleton type


### SourceFile
<pre><code class="language-scala" >type SourceFile: Nothing <: AnyRef</pre></code>
Scala source file


### Statement
<pre><code class="language-scala" >type Statement: Nothing <: Tree</pre></code>
Tree representing a statement in the source code


### Super
<pre><code class="language-scala" >type Super: Nothing <: Term</pre></code>
Tree representing `super` in the source code


### SuperType
<pre><code class="language-scala" >type SuperType: Nothing <: Type</pre></code>
Type of a `super` refernce


### SymRef
<pre><code class="language-scala" >type SymRef: Nothing <: Type</pre></code>
Type of a reference to a symbol


### Symbol
<pre><code class="language-scala" >type Symbol: Nothing <: AnyRef</pre></code>
Symbol of a definition.
Then can be compared with == to know if the definition is the same.


### Term
<pre><code class="language-scala" >type Term: Nothing <: Statement</pre></code>
Tree representing an expression in the source code


### TermRef
<pre><code class="language-scala" >type TermRef: Nothing <: Type</pre></code>
Type of a reference to a term


### TermSymbol
<pre><code class="language-scala" >type TermSymbol: Nothing <: Symbol</pre></code>

### This
<pre><code class="language-scala" >type This: Nothing <: Term</pre></code>
Tree representing `this` in the source code


### ThisType
<pre><code class="language-scala" >type ThisType: Nothing <: Type</pre></code>
Type of `this`


### Tree
<pre><code class="language-scala" >type Tree: Nothing <: AnyRef</pre></code>
Tree representing code written in the source


### Try
<pre><code class="language-scala" >type Try: Nothing <: Term</pre></code>
Tree representing a tyr catch `try x catch { ... } finally { ... }` in the source code


### Type
<pre><code class="language-scala" >type Type: Nothing <: TypeOrBounds</pre></code>
A type


### TypeApply
<pre><code class="language-scala" >type TypeApply: Nothing <: Term</pre></code>
Tree an application of type arguments


### TypeBind
<pre><code class="language-scala" >type TypeBind: Nothing <: TypeTree</pre></code>
Type tree representing a type binding


### TypeBindSymbol
<pre><code class="language-scala" >type TypeBindSymbol: Nothing <: TypeSymbol</pre></code>
Symbol representing a bind definition.


### TypeBlock
<pre><code class="language-scala" >type TypeBlock: Nothing <: TypeTree</pre></code>
Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }`


### TypeBounds
<pre><code class="language-scala" >type TypeBounds: Nothing <: TypeOrBounds</pre></code>
Type bounds


### TypeBoundsTree
<pre><code class="language-scala" >type TypeBoundsTree: Nothing <: Tree</pre></code>
Type tree representing a type bound written in the source


### TypeCaseDef
<pre><code class="language-scala" >type TypeCaseDef: Nothing <: Tree</pre></code>
Branch of a type pattern match


### TypeDef
<pre><code class="language-scala" >type TypeDef: Nothing <: Definition</pre></code>
Tree representing a type (paramter or member) definition in the source code


### TypeDefSymbol
<pre><code class="language-scala" >type TypeDefSymbol: Nothing <: TypeSymbol</pre></code>
Symbol of a type (parameter or member) definition.


### TypeIdent
<pre><code class="language-scala" >type TypeIdent: Nothing <: TypeTree</pre></code>
Type tree representing a reference to definition with a given name


### TypeLambda
<pre><code class="language-scala" >type TypeLambda: Nothing <: LambdaType[TypeBounds]</pre></code>
Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda.


### TypeOrBounds
<pre><code class="language-scala" >type TypeOrBounds: Nothing <: AnyRef</pre></code>
Type or bounds


### TypeRef
<pre><code class="language-scala" >type TypeRef: Nothing <: Type</pre></code>
Type of a reference to a type


### TypeSelect
<pre><code class="language-scala" >type TypeSelect: Nothing <: TypeTree</pre></code>
Type tree representing a selection of definition with a given name on a given term prefix


### TypeSymbol
<pre><code class="language-scala" >type TypeSymbol: Nothing <: Symbol</pre></code>

### TypeTest
<pre><code class="language-scala" >type TypeTest: Nothing <: Pattern</pre></code>
Pattern representing a `x: Y` type test.


### TypeTree
<pre><code class="language-scala" >type TypeTree: Nothing <: Tree</pre></code>
Type tree representing a type written in the source


### Typed
<pre><code class="language-scala" >type Typed: Nothing <: Term</pre></code>
Tree representing a type ascription `x: T` in the source code


### Unapply
<pre><code class="language-scala" >type Unapply: Nothing <: Pattern</pre></code>
Pattern representing a `Xyz(...)` unapply.


### ValDef
<pre><code class="language-scala" >type ValDef: Nothing <: Definition</pre></code>
Tree representing a value definition in the source code This inclues `val`, `lazy val`, `var`, `object` and parameter definitions.


### ValDefSymbol
<pre><code class="language-scala" >type ValDefSymbol: Nothing <: TermSymbol</pre></code>
Symbol representing a value definition. This includes `val`, `lazy val`, `var`, `object` and parameter definitions.


### Value
<pre><code class="language-scala" >type Value: Nothing <: Pattern</pre></code>
Pattern representing a value. This includes `1`, ```x``` and `_`


### While
<pre><code class="language-scala" >type While: Nothing <: Term</pre></code>
Tree representing a while loop


### WildcardPattern
<pre><code class="language-scala" >type WildcardPattern: Nothing <: Pattern</pre></code>
Pattern representing a `_` pattern


### WildcardTypeTree
<pre><code class="language-scala" >type WildcardTypeTree: Nothing <: Tree</pre></code>
Type tree representing wildcard type bounds written in the source.
The wildcard type `_` (for example in in `List[_]`) will be a type tree that
represents a type but has `TypeBound`a inside.


## Concrete Value Members:
### AndType_left
<pre><code class="language-scala" >def AndType_left(self: AndType)(implicit ctx: Context): Type</pre></code>

### AndType_right
<pre><code class="language-scala" >def AndType_right(self: AndType)(implicit ctx: Context): Type</pre></code>

### AnnotatedType_annot
<pre><code class="language-scala" >def AnnotatedType_annot(self: AnnotatedType)(implicit ctx: Context): Term</pre></code>

### AnnotatedType_underlying
<pre><code class="language-scala" >def AnnotatedType_underlying(self: AnnotatedType)(implicit ctx: Context): Type</pre></code>

### Annotated_annotation
<pre><code class="language-scala" >def Annotated_annotation(self: Annotated)(implicit ctx: Context): Term</pre></code>

### Annotated_apply
<pre><code class="language-scala" >def Annotated_apply(arg: TypeTree, annotation: Term)(implicit ctx: Context): Annotated</pre></code>

### Annotated_arg
<pre><code class="language-scala" >def Annotated_arg(self: Annotated)(implicit ctx: Context): TypeTree</pre></code>

### Annotated_copy
<pre><code class="language-scala" >def Annotated_copy(original: Annotated)(arg: TypeTree, annotation: Term)(implicit ctx: Context): Annotated</pre></code>

### AppliedType_args
<pre><code class="language-scala" >def AppliedType_args(self: AppliedType)(implicit ctx: Context): List[TypeOrBounds]</pre></code>

### AppliedType_tycon
<pre><code class="language-scala" >def AppliedType_tycon(self: AppliedType)(implicit ctx: Context): Type</pre></code>

### Applied_apply
<pre><code class="language-scala" >def Applied_apply(tpt: TypeTree, args: List[Tree])(implicit ctx: Context): Applied</pre></code>

### Applied_args
<pre><code class="language-scala" >def Applied_args(self: Applied)(implicit ctx: Context): List[Tree]</pre></code>

### Applied_copy
<pre><code class="language-scala" >def Applied_copy(original: Applied)(tpt: TypeTree, args: List[Tree])(implicit ctx: Context): Applied</pre></code>

### Applied_tpt
<pre><code class="language-scala" >def Applied_tpt(self: Applied)(implicit ctx: Context): TypeTree</pre></code>

### Apply_apply
<pre><code class="language-scala" >def Apply_apply(fn: Term, args: List[Term])(implicit ctx: Context): Apply</pre></code>

### Apply_args
<pre><code class="language-scala" >def Apply_args(self: Apply)(implicit ctx: Context): List[Term]</pre></code>

### Apply_copy
<pre><code class="language-scala" >def Apply_copy(original: Tree)(fun: Term, args: List[Term])(implicit ctx: Context): Apply</pre></code>

### Apply_fun
<pre><code class="language-scala" >def Apply_fun(self: Apply)(implicit ctx: Context): Term</pre></code>

### Assign_apply
<pre><code class="language-scala" >def Assign_apply(lhs: Term, rhs: Term)(implicit ctx: Context): Assign</pre></code>

### Assign_copy
<pre><code class="language-scala" >def Assign_copy(original: Tree)(lhs: Term, rhs: Term)(implicit ctx: Context): Assign</pre></code>

### Assign_lhs
<pre><code class="language-scala" >def Assign_lhs(self: Assign)(implicit ctx: Context): Term</pre></code>

### Assign_rhs
<pre><code class="language-scala" >def Assign_rhs(self: Assign)(implicit ctx: Context): Term</pre></code>

### BindSymbol_tree
<pre><code class="language-scala" >def BindSymbol_tree(self: BindSymbol)(implicit ctx: Context): Bind</pre></code>
Bind pattern of this definition

### Block_apply
<pre><code class="language-scala" >def Block_apply(stats: List[Statement], expr: Term)(implicit ctx: Context): Block</pre></code>

### Block_copy
<pre><code class="language-scala" >def Block_copy(original: Tree)(stats: List[Statement], expr: Term)(implicit ctx: Context): Block</pre></code>

### Block_expr
<pre><code class="language-scala" >def Block_expr(self: Block)(implicit ctx: Context): Term</pre></code>

### Block_statements
<pre><code class="language-scala" >def Block_statements(self: Block)(implicit ctx: Context): List[Statement]</pre></code>

### ByNameType_underlying
<pre><code class="language-scala" >def ByNameType_underlying(self: ByNameType)(implicit ctx: Context): Type</pre></code>

### ByName_apply
<pre><code class="language-scala" >def ByName_apply(result: TypeTree)(implicit ctx: Context): ByName</pre></code>

### ByName_copy
<pre><code class="language-scala" >def ByName_copy(original: ByName)(result: TypeTree)(implicit ctx: Context): ByName</pre></code>

### ByName_result
<pre><code class="language-scala" >def ByName_result(self: ByName)(implicit ctx: Context): TypeTree</pre></code>

### CaseDef_guard
<pre><code class="language-scala" >def CaseDef_guard(self: CaseDef)(implicit ctx: Context): Option[Term]</pre></code>

### CaseDef_module_apply
<pre><code class="language-scala" >def CaseDef_module_apply(pattern: Pattern, guard: Option[Term], body: Term)(implicit ctx: Context): CaseDef</pre></code>

### CaseDef_module_copy
<pre><code class="language-scala" >def CaseDef_module_copy(original: CaseDef)(pattern: Pattern, guard: Option[Term], body: Term)(implicit ctx: Context): CaseDef</pre></code>

### CaseDef_pattern
<pre><code class="language-scala" >def CaseDef_pattern(self: CaseDef)(implicit ctx: Context): Pattern</pre></code>

### CaseDef_rhs
<pre><code class="language-scala" >def CaseDef_rhs(self: CaseDef)(implicit ctx: Context): Term</pre></code>

### ClassDefSymbol_caseFields
<pre><code class="language-scala" >def ClassDefSymbol_caseFields(self: Symbol)(implicit ctx: Context): List[ValDefSymbol]</pre></code>
Fields of a case class type -- only the ones declared in primary constructor

### ClassDefSymbol_classMethod
<pre><code class="language-scala" >def ClassDefSymbol_classMethod(self: Symbol)(name: String)(implicit ctx: Context): List[DefDefSymbol]</pre></code>
Get non-private named methods defined directly inside the class

### ClassDefSymbol_classMethods
<pre><code class="language-scala" >def ClassDefSymbol_classMethods(self: Symbol)(implicit ctx: Context): List[DefDefSymbol]</pre></code>
Get all non-private methods defined directly inside the class, excluding constructors

### ClassDefSymbol_companionClass
<pre><code class="language-scala" >def ClassDefSymbol_companionClass(self: Symbol)(implicit ctx: Context): Option[ClassDefSymbol]</pre></code>
The class symbol of the companion module class

### ClassDefSymbol_companionModule
<pre><code class="language-scala" >def ClassDefSymbol_companionModule(self: Symbol)(implicit ctx: Context): Option[ValDefSymbol]</pre></code>
The symbol of the companion module

### ClassDefSymbol_field
<pre><code class="language-scala" >def ClassDefSymbol_field(self: Symbol)(name: String)(implicit ctx: Context): Option[Symbol]</pre></code>
Field with the given name directly declared in the class

### ClassDefSymbol_fields
<pre><code class="language-scala" >def ClassDefSymbol_fields(self: Symbol)(implicit ctx: Context): List[Symbol]</pre></code>
Fields directly declared in the class

### ClassDefSymbol_method
<pre><code class="language-scala" >def ClassDefSymbol_method(self: Symbol)(name: String)(implicit ctx: Context): List[DefDefSymbol]</pre></code>
Get named non-private methods declared or inherited

### ClassDefSymbol_methods
<pre><code class="language-scala" >def ClassDefSymbol_methods(self: Symbol)(implicit ctx: Context): List[DefDefSymbol]</pre></code>
Get all non-private methods declared or inherited

### ClassDefSymbol_moduleClass
<pre><code class="language-scala" >def ClassDefSymbol_moduleClass(self: Symbol)(implicit ctx: Context): Option[Symbol]</pre></code>
The symbol of the class of the companion module

### ClassDefSymbol_of
<pre><code class="language-scala" >def ClassDefSymbol_of(fullName: String)(implicit ctx: Context): ClassDefSymbol</pre></code>

### ClassDefSymbol_tree
<pre><code class="language-scala" >def ClassDefSymbol_tree(self: ClassDefSymbol)(implicit ctx: Context): ClassDef</pre></code>
ClassDef tree of this definition

### ClassDef_body
<pre><code class="language-scala" >def ClassDef_body(self: ClassDef)(implicit ctx: Context): List[Statement]</pre></code>

### ClassDef_constructor
<pre><code class="language-scala" >def ClassDef_constructor(self: ClassDef)(implicit ctx: Context): DefDef</pre></code>

### ClassDef_copy
<pre><code class="language-scala" >def ClassDef_copy(original: ClassDef)(name: String, constr: DefDef, parents: List[Tree], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement])(implicit ctx: Context): ClassDef</pre></code>

### ClassDef_derived
<pre><code class="language-scala" >def ClassDef_derived(self: ClassDef)(implicit ctx: Context): List[TypeTree]</pre></code>

### ClassDef_parents
<pre><code class="language-scala" >def ClassDef_parents(self: ClassDef)(implicit ctx: Context): List[Tree]</pre></code>

### ClassDef_self
<pre><code class="language-scala" >def ClassDef_self(self: ClassDef)(implicit ctx: Context): Option[ValDef]</pre></code>

### ClassDef_symbol
<pre><code class="language-scala" >def ClassDef_symbol(self: ClassDef)(implicit ctx: Context): ClassDefSymbol</pre></code>

### Comment_expanded
<pre><code class="language-scala" >def Comment_expanded(self: Comment): Option[String]</pre></code>

### Comment_raw
<pre><code class="language-scala" >def Comment_raw(self: Comment): String</pre></code>

### Comment_usecases
<pre><code class="language-scala" >def Comment_usecases(self: Comment): List[(String, Option[DefDef])]</pre></code>

### ConstantType_constant
<pre><code class="language-scala" >def ConstantType_constant(self: ConstantType)(implicit ctx: Context): Constant</pre></code>

### Constant_Boolean_apply
<pre><code class="language-scala" >def Constant_Boolean_apply(x: Boolean): Constant</pre></code>

### Constant_Byte_apply
<pre><code class="language-scala" >def Constant_Byte_apply(x: Byte): Constant</pre></code>

### Constant_Char_apply
<pre><code class="language-scala" >def Constant_Char_apply(x: Char): Constant</pre></code>

### Constant_ClassTag_apply
<pre><code class="language-scala" >def Constant_ClassTag_apply(x: <a href="../../reflect/ClassTag.md">ClassTag</a>[Nothing <: Any]): Constant</pre></code>

### Constant_Double_apply
<pre><code class="language-scala" >def Constant_Double_apply(x: Double): Constant</pre></code>

### Constant_Float_apply
<pre><code class="language-scala" >def Constant_Float_apply(x: Float): Constant</pre></code>

### Constant_Int_apply
<pre><code class="language-scala" >def Constant_Int_apply(x: Int): Constant</pre></code>

### Constant_Long_apply
<pre><code class="language-scala" >def Constant_Long_apply(x: Long): Constant</pre></code>

### Constant_Null_apply
<pre><code class="language-scala" >def Constant_Null_apply(): Constant</pre></code>

### Constant_Short_apply
<pre><code class="language-scala" >def Constant_Short_apply(x: Short): Constant</pre></code>

### Constant_String_apply
<pre><code class="language-scala" >def Constant_String_apply(x: String): Constant</pre></code>

### Constant_Symbol_apply
<pre><code class="language-scala" >def Constant_Symbol_apply(x: Symbol): Constant</pre></code>

### Constant_Unit_apply
<pre><code class="language-scala" >def Constant_Unit_apply(): Constant</pre></code>

### Constant_value
<pre><code class="language-scala" >def Constant_value(const: Constant): Any</pre></code>

### Context_owner
<pre><code class="language-scala" >def Context_owner(self: Context): Symbol</pre></code>
Returns the owner of the context

### Context_printColors
<pre><code class="language-scala" >def Context_printColors(self: Context): Boolean</pre></code>
Returns true if the generated strings are allowed to use colors

### Context_source
<pre><code class="language-scala" >def Context_source(self: Context): Path</pre></code>
Returns the source file being compiled. The path is relative to the current working directory.

### Context_withColors
<pre><code class="language-scala" >def Context_withColors(self: Context): Context</pre></code>
Returns a new context where printColors is true

### Context_withoutColors
<pre><code class="language-scala" >def Context_withoutColors(self: Context): Context</pre></code>
Returns a new context where printColors is false

### DefDefSymbol_signature
<pre><code class="language-scala" >def DefDefSymbol_signature(self: DefDefSymbol)(implicit ctx: Context): Signature</pre></code>
Signature of this definition

### DefDefSymbol_tree
<pre><code class="language-scala" >def DefDefSymbol_tree(self: DefDefSymbol)(implicit ctx: Context): DefDef</pre></code>
DefDef tree of this definition

### DefDef_apply
<pre><code class="language-scala" >def DefDef_apply(symbol: DefDefSymbol, rhsFn: (List[Type]) => (List[List[Term]]) => Option[Term])(implicit ctx: Context): DefDef</pre></code>

### DefDef_copy
<pre><code class="language-scala" >def DefDef_copy(original: DefDef)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term])(implicit ctx: Context): DefDef</pre></code>

### DefDef_paramss
<pre><code class="language-scala" >def DefDef_paramss(self: DefDef)(implicit ctx: Context): List[List[ValDef]]</pre></code>

### DefDef_returnTpt
<pre><code class="language-scala" >def DefDef_returnTpt(self: DefDef)(implicit ctx: Context): TypeTree</pre></code>

### DefDef_rhs
<pre><code class="language-scala" >def DefDef_rhs(self: DefDef)(implicit ctx: Context): Option[Term]</pre></code>

### DefDef_symbol
<pre><code class="language-scala" >def DefDef_symbol(self: DefDef)(implicit ctx: Context): DefDefSymbol</pre></code>

### DefDef_typeParams
<pre><code class="language-scala" >def DefDef_typeParams(self: DefDef)(implicit ctx: Context): List[TypeDef]</pre></code>

### Definition_name
<pre><code class="language-scala" >def Definition_name(self: Definition)(implicit ctx: Context): String</pre></code>

### Definitions_AnyClass
<pre><code class="language-scala" >def Definitions_AnyClass: Symbol</pre></code>

### Definitions_AnyRefClass
<pre><code class="language-scala" >def Definitions_AnyRefClass: Symbol</pre></code>

### Definitions_AnyRefType
<pre><code class="language-scala" >def Definitions_AnyRefType: Type</pre></code>

### Definitions_AnyType
<pre><code class="language-scala" >def Definitions_AnyType: Type</pre></code>

### Definitions_AnyValClass
<pre><code class="language-scala" >def Definitions_AnyValClass: Symbol</pre></code>

### Definitions_AnyValType
<pre><code class="language-scala" >def Definitions_AnyValType: Type</pre></code>

### Definitions_ArrayClass
<pre><code class="language-scala" >def Definitions_ArrayClass: Symbol</pre></code>

### Definitions_ArrayModule
<pre><code class="language-scala" >def Definitions_ArrayModule: Symbol</pre></code>

### Definitions_Array_apply
<pre><code class="language-scala" >def Definitions_Array_apply: Symbol</pre></code>

### Definitions_Array_clone
<pre><code class="language-scala" >def Definitions_Array_clone: Symbol</pre></code>

### Definitions_Array_length
<pre><code class="language-scala" >def Definitions_Array_length: Symbol</pre></code>

### Definitions_Array_update
<pre><code class="language-scala" >def Definitions_Array_update: Symbol</pre></code>

### Definitions_BooleanClass
<pre><code class="language-scala" >def Definitions_BooleanClass: Symbol</pre></code>

### Definitions_BooleanType
<pre><code class="language-scala" >def Definitions_BooleanType: Type</pre></code>

### Definitions_ByteClass
<pre><code class="language-scala" >def Definitions_ByteClass: Symbol</pre></code>

### Definitions_ByteType
<pre><code class="language-scala" >def Definitions_ByteType: Type</pre></code>

### Definitions_CharClass
<pre><code class="language-scala" >def Definitions_CharClass: Symbol</pre></code>

### Definitions_CharType
<pre><code class="language-scala" >def Definitions_CharType: Type</pre></code>

### Definitions_ClassClass
<pre><code class="language-scala" >def Definitions_ClassClass: Symbol</pre></code>

### Definitions_DoubleClass
<pre><code class="language-scala" >def Definitions_DoubleClass: Symbol</pre></code>

### Definitions_DoubleType
<pre><code class="language-scala" >def Definitions_DoubleType: Type</pre></code>

### Definitions_EmptyPackageClass
<pre><code class="language-scala" >def Definitions_EmptyPackageClass: Symbol</pre></code>

### Definitions_FloatClass
<pre><code class="language-scala" >def Definitions_FloatClass: Symbol</pre></code>

### Definitions_FloatType
<pre><code class="language-scala" >def Definitions_FloatType: Type</pre></code>

### Definitions_FunctionClass
<pre><code class="language-scala" >def Definitions_FunctionClass(arity: Int, isImplicit: Boolean, isErased: Boolean): Symbol</pre></code>

### Definitions_IntClass
<pre><code class="language-scala" >def Definitions_IntClass: Symbol</pre></code>

### Definitions_IntType
<pre><code class="language-scala" >def Definitions_IntType: Type</pre></code>

### Definitions_InternalQuoted_patternBindHoleAnnot
<pre><code class="language-scala" >def Definitions_InternalQuoted_patternBindHoleAnnot: Symbol</pre></code>
Symbol of scala.internal.Quoted.patternBindHole

### Definitions_InternalQuoted_patternHole
<pre><code class="language-scala" >def Definitions_InternalQuoted_patternHole: Symbol</pre></code>
Symbol of scala.internal.Quoted.patternHole

### Definitions_JavaLangPackage
<pre><code class="language-scala" >def Definitions_JavaLangPackage: Symbol</pre></code>

### Definitions_LongClass
<pre><code class="language-scala" >def Definitions_LongClass: Symbol</pre></code>

### Definitions_LongType
<pre><code class="language-scala" >def Definitions_LongType: Type</pre></code>

### Definitions_NoneModule
<pre><code class="language-scala" >def Definitions_NoneModule: Symbol</pre></code>

### Definitions_NothingClass
<pre><code class="language-scala" >def Definitions_NothingClass: Symbol</pre></code>

### Definitions_NothingType
<pre><code class="language-scala" >def Definitions_NothingType: Type</pre></code>

### Definitions_NullClass
<pre><code class="language-scala" >def Definitions_NullClass: Symbol</pre></code>

### Definitions_NullType
<pre><code class="language-scala" >def Definitions_NullType: Type</pre></code>

### Definitions_ObjectClass
<pre><code class="language-scala" >def Definitions_ObjectClass: Symbol</pre></code>

### Definitions_ObjectType
<pre><code class="language-scala" >def Definitions_ObjectType: Type</pre></code>

### Definitions_OptionClass
<pre><code class="language-scala" >def Definitions_OptionClass: Symbol</pre></code>

### Definitions_PredefModule
<pre><code class="language-scala" >def Definitions_PredefModule: Symbol</pre></code>

### Definitions_ProductClass
<pre><code class="language-scala" >def Definitions_ProductClass: Symbol</pre></code>

### Definitions_RepeatedParamClass
<pre><code class="language-scala" >def Definitions_RepeatedParamClass: ClassDefSymbol</pre></code>

### Definitions_RootClass
<pre><code class="language-scala" >def Definitions_RootClass: Symbol</pre></code>

### Definitions_RootPackage
<pre><code class="language-scala" >def Definitions_RootPackage: Symbol</pre></code>

### Definitions_ScalaPackage
<pre><code class="language-scala" >def Definitions_ScalaPackage: Symbol</pre></code>

### Definitions_ScalaPackageClass
<pre><code class="language-scala" >def Definitions_ScalaPackageClass: Symbol</pre></code>

### Definitions_ShortClass
<pre><code class="language-scala" >def Definitions_ShortClass: Symbol</pre></code>

### Definitions_ShortType
<pre><code class="language-scala" >def Definitions_ShortType: Type</pre></code>

### Definitions_SomeModule
<pre><code class="language-scala" >def Definitions_SomeModule: Symbol</pre></code>

### Definitions_StringClass
<pre><code class="language-scala" >def Definitions_StringClass: Symbol</pre></code>

### Definitions_StringType
<pre><code class="language-scala" >def Definitions_StringType: Type</pre></code>

### Definitions_TupleClass
<pre><code class="language-scala" >def Definitions_TupleClass(arity: Int): Symbol</pre></code>

### Definitions_UnitClass
<pre><code class="language-scala" >def Definitions_UnitClass: Symbol</pre></code>

### Definitions_UnitType
<pre><code class="language-scala" >def Definitions_UnitType: Type</pre></code>

### Flags_Abstract
<pre><code class="language-scala" >def Flags_Abstract: Flags</pre></code>

### Flags_Artifact
<pre><code class="language-scala" >def Flags_Artifact: Flags</pre></code>

### Flags_Case
<pre><code class="language-scala" >def Flags_Case: Flags</pre></code>

### Flags_CaseAcessor
<pre><code class="language-scala" >def Flags_CaseAcessor: Flags</pre></code>

### Flags_Contravariant
<pre><code class="language-scala" >def Flags_Contravariant: Flags</pre></code>

### Flags_Covariant
<pre><code class="language-scala" >def Flags_Covariant: Flags</pre></code>

### Flags_DefaultParameterized
<pre><code class="language-scala" >def Flags_DefaultParameterized: Flags</pre></code>

### Flags_EmptyFlags
<pre><code class="language-scala" >def Flags_EmptyFlags: Flags</pre></code>

### Flags_Enum
<pre><code class="language-scala" >def Flags_Enum: Flags</pre></code>

### Flags_Erased
<pre><code class="language-scala" >def Flags_Erased: Flags</pre></code>

### Flags_FieldAccessor
<pre><code class="language-scala" >def Flags_FieldAccessor: Flags</pre></code>

### Flags_Final
<pre><code class="language-scala" >def Flags_Final: Flags</pre></code>

### Flags_Given
<pre><code class="language-scala" >def Flags_Given: Flags</pre></code>

### Flags_Implicit
<pre><code class="language-scala" >def Flags_Implicit: Flags</pre></code>

### Flags_Implied
<pre><code class="language-scala" >def Flags_Implied: Flags</pre></code>

### Flags_Inline
<pre><code class="language-scala" >def Flags_Inline: Flags</pre></code>

### Flags_JavaDefined
<pre><code class="language-scala" >def Flags_JavaDefined: Flags</pre></code>

### Flags_Lazy
<pre><code class="language-scala" >def Flags_Lazy: Flags</pre></code>

### Flags_Local
<pre><code class="language-scala" >def Flags_Local: Flags</pre></code>

### Flags_Macro
<pre><code class="language-scala" >def Flags_Macro: Flags</pre></code>

### Flags_ModuleClass
<pre><code class="language-scala" >def Flags_ModuleClass: Flags</pre></code>

### Flags_Mutable
<pre><code class="language-scala" >def Flags_Mutable: Flags</pre></code>

### Flags_Object
<pre><code class="language-scala" >def Flags_Object: Flags</pre></code>

### Flags_Override
<pre><code class="language-scala" >def Flags_Override: Flags</pre></code>

### Flags_Package
<pre><code class="language-scala" >def Flags_Package: Flags</pre></code>

### Flags_Param
<pre><code class="language-scala" >def Flags_Param: Flags</pre></code>

### Flags_ParamAccessor
<pre><code class="language-scala" >def Flags_ParamAccessor: Flags</pre></code>

### Flags_Private
<pre><code class="language-scala" >def Flags_Private: Flags</pre></code>

### Flags_PrivateLocal
<pre><code class="language-scala" >def Flags_PrivateLocal: Flags</pre></code>

### Flags_Protected
<pre><code class="language-scala" >def Flags_Protected: Flags</pre></code>

### Flags_Scala2X
<pre><code class="language-scala" >def Flags_Scala2X: Flags</pre></code>

### Flags_Sealed
<pre><code class="language-scala" >def Flags_Sealed: Flags</pre></code>

### Flags_StableRealizable
<pre><code class="language-scala" >def Flags_StableRealizable: Flags</pre></code>

### Flags_Static
<pre><code class="language-scala" >def Flags_Static: Flags</pre></code>

### Flags_Synthetic
<pre><code class="language-scala" >def Flags_Synthetic: Flags</pre></code>

### Flags_Trait
<pre><code class="language-scala" >def Flags_Trait: Flags</pre></code>

### Flags_and
<pre><code class="language-scala" >def Flags_and(self: Flags)(that: Flags): Flags</pre></code>
Intersection of the two flag sets

### Flags_is
<pre><code class="language-scala" >def Flags_is(self: Flags)(that: Flags): Boolean</pre></code>
Is the given flag set a subset of this flag sets

### Flags_or
<pre><code class="language-scala" >def Flags_or(self: Flags)(that: Flags): Flags</pre></code>
Union of the two flag sets

### Id_name
<pre><code class="language-scala" >def Id_name(self: Id)(implicit ctx: Context): String</pre></code>
Name of the identifier

### Id_pos
<pre><code class="language-scala" >def Id_pos(self: Id)(implicit ctx: Context): Position</pre></code>
Position in the source code

### Ident_apply
<pre><code class="language-scala" >def Ident_apply(tmref: TermRef)(implicit ctx: Context): Term</pre></code>

### Ident_copy
<pre><code class="language-scala" >def Ident_copy(original: Tree)(name: String)(implicit ctx: Context): Ident</pre></code>

### Ident_name
<pre><code class="language-scala" >def Ident_name(self: Ident)(implicit ctx: Context): String</pre></code>

### If_apply
<pre><code class="language-scala" >def If_apply(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If</pre></code>

### If_cond
<pre><code class="language-scala" >def If_cond(self: If)(implicit ctx: Context): Term</pre></code>

### If_copy
<pre><code class="language-scala" >def If_copy(original: Tree)(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If</pre></code>

### If_elsep
<pre><code class="language-scala" >def If_elsep(self: If)(implicit ctx: Context): Term</pre></code>

### If_thenp
<pre><code class="language-scala" >def If_thenp(self: If)(implicit ctx: Context): Term</pre></code>

### ImplicitMatch_apply
<pre><code class="language-scala" >def ImplicitMatch_apply(cases: List[CaseDef])(implicit ctx: Context): ImpliedMatch</pre></code>

### ImplicitMatch_cases
<pre><code class="language-scala" >def ImplicitMatch_cases(self: ImpliedMatch)(implicit ctx: Context): List[CaseDef]</pre></code>

### ImplicitMatch_copy
<pre><code class="language-scala" >def ImplicitMatch_copy(original: Tree)(cases: List[CaseDef])(implicit ctx: Context): ImpliedMatch</pre></code>

### Import_apply
<pre><code class="language-scala" >def Import_apply(importImplied: Boolean, expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import</pre></code>

### Import_copy
<pre><code class="language-scala" >def Import_copy(original: Import)(importImplied: Boolean, expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import</pre></code>

### Import_expr
<pre><code class="language-scala" >def Import_expr(self: Import)(implicit ctx: Context): Term</pre></code>

### Import_implied
<pre><code class="language-scala" >def Import_implied(self: Import): Boolean</pre></code>

### Import_selectors
<pre><code class="language-scala" >def Import_selectors(self: Import)(implicit ctx: Context): List[ImportSelector]</pre></code>

### Inferred_apply
<pre><code class="language-scala" >def Inferred_apply(tpe: Type)(implicit ctx: Context): Inferred</pre></code>

### Inlined_apply
<pre><code class="language-scala" >def Inlined_apply(call: Option[Tree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined</pre></code>

### Inlined_bindings
<pre><code class="language-scala" >def Inlined_bindings(self: Inlined)(implicit ctx: Context): List[Definition]</pre></code>

### Inlined_body
<pre><code class="language-scala" >def Inlined_body(self: Inlined)(implicit ctx: Context): Term</pre></code>

### Inlined_call
<pre><code class="language-scala" >def Inlined_call(self: Inlined)(implicit ctx: Context): Option[Tree]</pre></code>

### Inlined_copy
<pre><code class="language-scala" >def Inlined_copy(original: Tree)(call: Option[Tree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined</pre></code>

### Lambda_apply
<pre><code class="language-scala" >def Lambda_apply(meth: Term, tpt: Option[TypeTree])(implicit ctx: Context): Lambda</pre></code>

### Lambda_copy
<pre><code class="language-scala" >def Lambda_copy(original: Tree)(meth: Tree, tpt: Option[TypeTree])(implicit ctx: Context): Lambda</pre></code>

### Lambda_meth
<pre><code class="language-scala" >def Lambda_meth(self: Lambda)(implicit ctx: Context): Term</pre></code>

### Lambda_tptOpt
<pre><code class="language-scala" >def Lambda_tptOpt(self: Lambda)(implicit ctx: Context): Option[TypeTree]</pre></code>

### Lambdaapply
<pre><code class="language-scala" >def Lambdaapply(tparams: List[TypeDef], body: Tree)(implicit ctx: Context): LambdaTypeTree</pre></code>

### Lambdabody
<pre><code class="language-scala" >def Lambdabody(self: LambdaTypeTree)(implicit ctx: Context): Tree</pre></code>

### Lambdacopy
<pre><code class="language-scala" >def Lambdacopy(original: LambdaTypeTree)(tparams: List[TypeDef], body: Tree)(implicit ctx: Context): LambdaTypeTree</pre></code>

### Lambdatparams
<pre><code class="language-scala" >def Lambdatparams(self: LambdaTypeTree)(implicit ctx: Context): List[TypeDef]</pre></code>

### Literal_apply
<pre><code class="language-scala" >def Literal_apply(constant: Constant)(implicit ctx: Context): Literal</pre></code>

### Literal_constant
<pre><code class="language-scala" >def Literal_constant(self: Literal)(implicit ctx: Context): Constant</pre></code>

### Literal_copy
<pre><code class="language-scala" >def Literal_copy(original: Tree)(constant: Constant)(implicit ctx: Context): Literal</pre></code>

### MatchTypeTree_apply
<pre><code class="language-scala" >def MatchTypeTree_apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): MatchTypeTree</pre></code>

### MatchTypeTree_bound
<pre><code class="language-scala" >def MatchTypeTree_bound(self: MatchTypeTree)(implicit ctx: Context): Option[TypeTree]</pre></code>

### MatchTypeTree_cases
<pre><code class="language-scala" >def MatchTypeTree_cases(self: MatchTypeTree)(implicit ctx: Context): List[TypeCaseDef]</pre></code>

### MatchTypeTree_copy
<pre><code class="language-scala" >def MatchTypeTree_copy(original: MatchTypeTree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(implicit ctx: Context): MatchTypeTree</pre></code>

### MatchTypeTree_selector
<pre><code class="language-scala" >def MatchTypeTree_selector(self: MatchTypeTree)(implicit ctx: Context): TypeTree</pre></code>

### MatchType_bound
<pre><code class="language-scala" >def MatchType_bound(self: MatchType)(implicit ctx: Context): Type</pre></code>

### MatchType_cases
<pre><code class="language-scala" >def MatchType_cases(self: MatchType)(implicit ctx: Context): List[Type]</pre></code>

### MatchType_scrutinee
<pre><code class="language-scala" >def MatchType_scrutinee(self: MatchType)(implicit ctx: Context): Type</pre></code>

### Match_apply
<pre><code class="language-scala" >def Match_apply(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match</pre></code>

### Match_cases
<pre><code class="language-scala" >def Match_cases(self: Match)(implicit ctx: Context): List[CaseDef]</pre></code>

### Match_copy
<pre><code class="language-scala" >def Match_copy(original: Tree)(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match</pre></code>

### Match_scrutinee
<pre><code class="language-scala" >def Match_scrutinee(self: Match)(implicit ctx: Context): Term</pre></code>

### MethodType_isErased
<pre><code class="language-scala" >def MethodType_isErased(self: MethodType): Boolean</pre></code>

### MethodType_isImplicit
<pre><code class="language-scala" >def MethodType_isImplicit(self: MethodType): Boolean</pre></code>

### MethodType_paramNames
<pre><code class="language-scala" >def MethodType_paramNames(self: MethodType)(implicit ctx: Context): List[String]</pre></code>

### MethodType_paramTypes
<pre><code class="language-scala" >def MethodType_paramTypes(self: MethodType)(implicit ctx: Context): List[Type]</pre></code>

### MethodType_resType
<pre><code class="language-scala" >def MethodType_resType(self: MethodType)(implicit ctx: Context): Type</pre></code>

### NamedArg_apply
<pre><code class="language-scala" >def NamedArg_apply(name: String, arg: Term)(implicit ctx: Context): NamedArg</pre></code>

### NamedArg_copy
<pre><code class="language-scala" >def NamedArg_copy(tree: NamedArg)(name: String, arg: Term)(implicit ctx: Context): NamedArg</pre></code>

### NamedArg_name
<pre><code class="language-scala" >def NamedArg_name(self: NamedArg)(implicit ctx: Context): String</pre></code>

### NamedArg_value
<pre><code class="language-scala" >def NamedArg_value(self: NamedArg)(implicit ctx: Context): Term</pre></code>

### New_apply
<pre><code class="language-scala" >def New_apply(tpt: TypeTree)(implicit ctx: Context): New</pre></code>

### New_copy
<pre><code class="language-scala" >def New_copy(original: Tree)(tpt: TypeTree)(implicit ctx: Context): New</pre></code>

### New_tpt
<pre><code class="language-scala" >def New_tpt(self: New)(implicit ctx: Context): TypeTree</pre></code>

### OrType_left
<pre><code class="language-scala" >def OrType_left(self: OrType)(implicit ctx: Context): Type</pre></code>

### OrType_right
<pre><code class="language-scala" >def OrType_right(self: OrType)(implicit ctx: Context): Type</pre></code>

### PackageClause_apply
<pre><code class="language-scala" >def PackageClause_apply(pid: Ref, stats: List[Tree])(implicit ctx: Context): PackageClause</pre></code>

### PackageClause_copy
<pre><code class="language-scala" >def PackageClause_copy(original: PackageClause)(pid: Ref, stats: List[Tree])(implicit ctx: Context): PackageClause</pre></code>

### PackageClause_pid
<pre><code class="language-scala" >def PackageClause_pid(self: PackageClause)(implicit ctx: Context): Ref</pre></code>

### PackageClause_stats
<pre><code class="language-scala" >def PackageClause_stats(self: PackageClause)(implicit ctx: Context): List[Tree]</pre></code>

### PackageDefSymbol_tree
<pre><code class="language-scala" >def PackageDefSymbol_tree(self: PackageDefSymbol)(implicit ctx: Context): PackageDef</pre></code>

### PackageDef_members
<pre><code class="language-scala" >def PackageDef_members(self: PackageDef)(implicit ctx: Context): List[Statement]</pre></code>

### PackageDef_owner
<pre><code class="language-scala" >def PackageDef_owner(self: PackageDef)(implicit ctx: Context): PackageDef</pre></code>

### PackageDef_symbol
<pre><code class="language-scala" >def PackageDef_symbol(self: PackageDef)(implicit ctx: Context): PackageDefSymbol</pre></code>

### ParamRef_binder
<pre><code class="language-scala" >def ParamRef_binder(self: ParamRef)(implicit ctx: Context): LambdaType[TypeOrBounds]</pre></code>

### ParamRef_paramNum
<pre><code class="language-scala" >def ParamRef_paramNum(self: ParamRef)(implicit ctx: Context): Int</pre></code>

### Pattern_Alternatives_module_apply
<pre><code class="language-scala" >def Pattern_Alternatives_module_apply(patterns: List[Pattern])(implicit ctx: Context): Alternatives</pre></code>

### Pattern_Alternatives_module_copy
<pre><code class="language-scala" >def Pattern_Alternatives_module_copy(original: Alternatives)(patterns: List[Pattern])(implicit ctx: Context): Alternatives</pre></code>

### Pattern_Alternatives_patterns
<pre><code class="language-scala" >def Pattern_Alternatives_patterns(self: Alternatives)(implicit ctx: Context): List[Pattern]</pre></code>

### Pattern_Bind_module_copy
<pre><code class="language-scala" >def Pattern_Bind_module_copy(original: Bind)(name: String, pattern: Pattern)(implicit ctx: Context): Bind</pre></code>

### Pattern_Bind_name
<pre><code class="language-scala" >def Pattern_Bind_name(self: Bind)(implicit ctx: Context): String</pre></code>

### Pattern_Bind_pattern
<pre><code class="language-scala" >def Pattern_Bind_pattern(self: Bind)(implicit ctx: Context): Pattern</pre></code>

### Pattern_TypeTest_module_apply
<pre><code class="language-scala" >def Pattern_TypeTest_module_apply(tpt: TypeTree)(implicit ctx: Context): TypeTest</pre></code>

### Pattern_TypeTest_module_copy
<pre><code class="language-scala" >def Pattern_TypeTest_module_copy(original: TypeTest)(tpt: TypeTree)(implicit ctx: Context): TypeTest</pre></code>

### Pattern_TypeTest_tpt
<pre><code class="language-scala" >def Pattern_TypeTest_tpt(self: TypeTest)(implicit ctx: Context): TypeTree</pre></code>

### Pattern_Unapply_fun
<pre><code class="language-scala" >def Pattern_Unapply_fun(self: Unapply)(implicit ctx: Context): Term</pre></code>

### Pattern_Unapply_implicits
<pre><code class="language-scala" >def Pattern_Unapply_implicits(self: Unapply)(implicit ctx: Context): List[Term]</pre></code>

### Pattern_Unapply_module_copy
<pre><code class="language-scala" >def Pattern_Unapply_module_copy(original: Unapply)(fun: Term, implicits: List[Term], patterns: List[Pattern])(implicit ctx: Context): Unapply</pre></code>

### Pattern_Unapply_patterns
<pre><code class="language-scala" >def Pattern_Unapply_patterns(self: Unapply)(implicit ctx: Context): List[Pattern]</pre></code>

### Pattern_Value_module_apply
<pre><code class="language-scala" >def Pattern_Value_module_apply(term: Term)(implicit ctx: Context): Value</pre></code>

### Pattern_Value_module_copy
<pre><code class="language-scala" >def Pattern_Value_module_copy(original: Value)(term: Term)(implicit ctx: Context): Value</pre></code>

### Pattern_Value_value
<pre><code class="language-scala" >def Pattern_Value_value(self: Value)(implicit ctx: Context): Term</pre></code>

### Pattern_WildcardPattern_module_apply
<pre><code class="language-scala" >def Pattern_WildcardPattern_module_apply(tpe: TypeOrBounds)(implicit ctx: Context): WildcardPattern</pre></code>

### Pattern_pos
<pre><code class="language-scala" >def Pattern_pos(self: Pattern)(implicit ctx: Context): Position</pre></code>

### Pattern_symbol
<pre><code class="language-scala" >def Pattern_symbol(self: Pattern)(implicit ctx: Context): Symbol</pre></code>

### Pattern_tpe
<pre><code class="language-scala" >def Pattern_tpe(self: Pattern)(implicit ctx: Context): Type</pre></code>

### PolyType_paramBounds
<pre><code class="language-scala" >def PolyType_paramBounds(self: PolyType)(implicit ctx: Context): List[TypeBounds]</pre></code>

### PolyType_paramNames
<pre><code class="language-scala" >def PolyType_paramNames(self: PolyType)(implicit ctx: Context): List[String]</pre></code>

### PolyType_resType
<pre><code class="language-scala" >def PolyType_resType(self: PolyType)(implicit ctx: Context): Type</pre></code>

### Position_end
<pre><code class="language-scala" >def Position_end(self: Position): Int</pre></code>
The end offset in the source file

### Position_endColumn
<pre><code class="language-scala" >def Position_endColumn(self: Position): Int</pre></code>
The end column in the source file

### Position_endLine
<pre><code class="language-scala" >def Position_endLine(self: Position): Int</pre></code>
The end line in the source file

### Position_exists
<pre><code class="language-scala" >def Position_exists(self: Position): Boolean</pre></code>
Does this position exist

### Position_sourceCode
<pre><code class="language-scala" >def Position_sourceCode(self: Position): String</pre></code>
Source code within the position

### Position_sourceFile
<pre><code class="language-scala" >def Position_sourceFile(self: Position): SourceFile</pre></code>
Source file in which this position is located

### Position_start
<pre><code class="language-scala" >def Position_start(self: Position): Int</pre></code>
The start offset in the source file

### Position_startColumn
<pre><code class="language-scala" >def Position_startColumn(self: Position): Int</pre></code>
The start column in the source file

### Position_startLine
<pre><code class="language-scala" >def Position_startLine(self: Position): Int</pre></code>
The start line in the source file

### Projection_copy
<pre><code class="language-scala" >def Projection_copy(original: Projection)(qualifier: TypeTree, name: String)(implicit ctx: Context): Projection</pre></code>

### Projection_name
<pre><code class="language-scala" >def Projection_name(self: Projection)(implicit ctx: Context): String</pre></code>

### Projection_qualifier
<pre><code class="language-scala" >def Projection_qualifier(self: Projection)(implicit ctx: Context): TypeTree</pre></code>

### QuotedExpr_cast
<pre><code class="language-scala" >def QuotedExpr_cast[U](self: <a href="../../quoted/Expr.md">Expr</a>[Nothing <: Any])(implicit tp: <a href="../../quoted/Type.md">Type</a>[U], ctx: Context): <a href="../../quoted/Expr.md">Expr</a>[U]</pre></code>
Checked cast to a `quoted.Expr[U]`

### QuotedExpr_seal
<pre><code class="language-scala" >def QuotedExpr_seal(self: Term)(implicit ctx: Context): <a href="../../quoted/Expr.md">Expr</a>[Any]</pre></code>
Convert `Term` to an `quoted.Expr[Any]`

### QuotedExpr_unseal
<pre><code class="language-scala" >def QuotedExpr_unseal(self: <a href="../../quoted/Expr.md">Expr</a>[Nothing <: Any])(implicit ctx: Context): Term</pre></code>
View this expression `quoted.Expr[_]` as a `Term`

### QuotedType_seal
<pre><code class="language-scala" >def QuotedType_seal(self: Type)(implicit ctx: Context): <a href="../../quoted/Type.md">Type</a>[Nothing <: AnyKind]</pre></code>
Convert `Type` to an `quoted.Type[_]`

### QuotedType_unseal
<pre><code class="language-scala" >def QuotedType_unseal(self: <a href="../../quoted/Type.md">Type</a>[Nothing <: AnyKind])(implicit ctx: Context): TypeTree</pre></code>
View this expression `quoted.Type[T]` as a `TypeTree`

### RecursiveThis_binder
<pre><code class="language-scala" >def RecursiveThis_binder(self: RecursiveThis)(implicit ctx: Context): RecursiveType</pre></code>

### RecursiveType_underlying
<pre><code class="language-scala" >def RecursiveType_underlying(self: RecursiveType)(implicit ctx: Context): Type</pre></code>

### Ref_apply
<pre><code class="language-scala" >def Ref_apply(sym: Symbol)(implicit ctx: Context): Ref</pre></code>

### Refined_copy
<pre><code class="language-scala" >def Refined_copy(original: Refined)(tpt: TypeTree, refinements: List[Definition])(implicit ctx: Context): Refined</pre></code>

### Refined_refinements
<pre><code class="language-scala" >def Refined_refinements(self: Refined)(implicit ctx: Context): List[Definition]</pre></code>

### Refined_tpt
<pre><code class="language-scala" >def Refined_tpt(self: Refined)(implicit ctx: Context): TypeTree</pre></code>

### Refinement_info
<pre><code class="language-scala" >def Refinement_info(self: Refinement)(implicit ctx: Context): TypeOrBounds</pre></code>

### Refinement_name
<pre><code class="language-scala" >def Refinement_name(self: Refinement)(implicit ctx: Context): String</pre></code>

### Refinement_parent
<pre><code class="language-scala" >def Refinement_parent(self: Refinement)(implicit ctx: Context): Type</pre></code>

### RenameSelector_from
<pre><code class="language-scala" >def RenameSelector_from(self: RenameSelector)(implicit ctx: Context): Id</pre></code>

### RenameSelector_to
<pre><code class="language-scala" >def RenameSelector_to(self: RenameSelector)(implicit ctx: Context): Id</pre></code>

### Repeated_apply
<pre><code class="language-scala" >def Repeated_apply(elems: List[Term], elemtpt: TypeTree)(implicit ctx: Context): Repeated</pre></code>

### Repeated_copy
<pre><code class="language-scala" >def Repeated_copy(original: Tree)(elems: List[Term], elemtpt: TypeTree)(implicit ctx: Context): Repeated</pre></code>

### Repeated_elems
<pre><code class="language-scala" >def Repeated_elems(self: Repeated)(implicit ctx: Context): List[Term]</pre></code>

### Repeated_elemtpt
<pre><code class="language-scala" >def Repeated_elemtpt(self: Repeated)(implicit ctx: Context): TypeTree</pre></code>

### Return_apply
<pre><code class="language-scala" >def Return_apply(expr: Term)(implicit ctx: Context): Return</pre></code>

### Return_copy
<pre><code class="language-scala" >def Return_copy(original: Tree)(expr: Term)(implicit ctx: Context): Return</pre></code>

### Return_expr
<pre><code class="language-scala" >def Return_expr(self: Return)(implicit ctx: Context): Term</pre></code>

### SelectOuter_apply
<pre><code class="language-scala" >def SelectOuter_apply(qualifier: Term, name: String, levels: Int)(implicit ctx: Context): SelectOuter</pre></code>

### SelectOuter_copy
<pre><code class="language-scala" >def SelectOuter_copy(original: Tree)(qualifier: Term, name: String, levels: Int)(implicit ctx: Context): SelectOuter</pre></code>

### SelectOuter_level
<pre><code class="language-scala" >def SelectOuter_level(self: SelectOuter)(implicit ctx: Context): Int</pre></code>

### SelectOuter_qualifier
<pre><code class="language-scala" >def SelectOuter_qualifier(self: SelectOuter)(implicit ctx: Context): Term</pre></code>

### SelectOuter_tpe
<pre><code class="language-scala" >def SelectOuter_tpe(self: SelectOuter)(implicit ctx: Context): Type</pre></code>

### Select_apply
<pre><code class="language-scala" >def Select_apply(qualifier: Term, symbol: Symbol)(implicit ctx: Context): Select</pre></code>

### Select_copy
<pre><code class="language-scala" >def Select_copy(original: Tree)(qualifier: Term, name: String)(implicit ctx: Context): Select</pre></code>

### Select_name
<pre><code class="language-scala" >def Select_name(self: Select)(implicit ctx: Context): String</pre></code>

### Select_overloaded
<pre><code class="language-scala" >def Select_overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term])(implicit ctx: Context): Apply</pre></code>

### Select_qualifier
<pre><code class="language-scala" >def Select_qualifier(self: Select)(implicit ctx: Context): Term</pre></code>

### Select_signature
<pre><code class="language-scala" >def Select_signature(self: Select)(implicit ctx: Context): Option[Signature]</pre></code>

### Select_unique
<pre><code class="language-scala" >def Select_unique(qualifier: Term, name: String)(implicit ctx: Context): Select</pre></code>

### Settings_color
<pre><code class="language-scala" >def Settings_color(self: Settings): Boolean</pre></code>

### Signature_paramSigs
<pre><code class="language-scala" >def Signature_paramSigs(self: Signature): List[String]</pre></code>
The (JVM) erased signatures of the parameters

### Signature_resultSig
<pre><code class="language-scala" >def Signature_resultSig(self: Signature): String</pre></code>
The (JVM) erased result type

### SimpleSelector_omited
<pre><code class="language-scala" >def SimpleSelector_omited(self: OmitSelector)(implicit ctx: Context): Id</pre></code>

### SimpleSelector_selection
<pre><code class="language-scala" >def SimpleSelector_selection(self: SimpleSelector)(implicit ctx: Context): Id</pre></code>

### Singleton_apply
<pre><code class="language-scala" >def Singleton_apply(ref: Term)(implicit ctx: Context): Singleton</pre></code>

### Singleton_copy
<pre><code class="language-scala" >def Singleton_copy(original: Singleton)(ref: Term)(implicit ctx: Context): Singleton</pre></code>

### Singleton_ref
<pre><code class="language-scala" >def Singleton_ref(self: Singleton)(implicit ctx: Context): Term</pre></code>

### SourceFile_content
<pre><code class="language-scala" >def SourceFile_content(self: SourceFile): String</pre></code>
Content of a source file

### SourceFile_jpath
<pre><code class="language-scala" >def SourceFile_jpath(self: SourceFile): Path</pre></code>
Path to a source file

### SuperType_supertpe
<pre><code class="language-scala" >def SuperType_supertpe(self: SuperType)(implicit ctx: Context): Type</pre></code>

### SuperType_thistpe
<pre><code class="language-scala" >def SuperType_thistpe(self: SuperType)(implicit ctx: Context): Type</pre></code>

### Super_apply
<pre><code class="language-scala" >def Super_apply(qual: Term, mix: Option[Id])(implicit ctx: Context): Super</pre></code>

### Super_copy
<pre><code class="language-scala" >def Super_copy(original: Tree)(qual: Term, mix: Option[Id])(implicit ctx: Context): Super</pre></code>

### Super_id
<pre><code class="language-scala" >def Super_id(self: Super)(implicit ctx: Context): Option[Id]</pre></code>

### Super_qualifier
<pre><code class="language-scala" >def Super_qualifier(self: Super)(implicit ctx: Context): Term</pre></code>

### SymRef_qualifier
<pre><code class="language-scala" >def SymRef_qualifier(self: SymRef)(implicit ctx: Context): TypeOrBounds</pre></code>

### Symbol_annots
<pre><code class="language-scala" >def Symbol_annots(self: Symbol)(implicit ctx: Context): List[Term]</pre></code>
Annotations attached to this symbol

### Symbol_comment
<pre><code class="language-scala" >def Symbol_comment(self: Symbol)(implicit ctx: Context): Option[Comment]</pre></code>
The comment of the symbol

### Symbol_flags
<pre><code class="language-scala" >def Symbol_flags(self: Symbol)(implicit ctx: Context): Flags</pre></code>
Flags of this symbol

### Symbol_fullName
<pre><code class="language-scala" >def Symbol_fullName(self: Symbol)(implicit ctx: Context): String</pre></code>
The full name of this symbol up to the root package.

### Symbol_isAbstractType
<pre><code class="language-scala" >def Symbol_isAbstractType(self: Symbol)(implicit ctx: Context): Boolean</pre></code>

### Symbol_isAliasType
<pre><code class="language-scala" >def Symbol_isAliasType(self: Symbol)(implicit ctx: Context): Boolean</pre></code>

### Symbol_isAnonymousClass
<pre><code class="language-scala" >def Symbol_isAnonymousClass(self: Symbol)(implicit ctx: Context): Boolean</pre></code>

### Symbol_isAnonymousFunction
<pre><code class="language-scala" >def Symbol_isAnonymousFunction(self: Symbol)(implicit ctx: Context): Boolean</pre></code>

### Symbol_isClassConstructor
<pre><code class="language-scala" >def Symbol_isClassConstructor(self: Symbol)(implicit ctx: Context): Boolean</pre></code>

### Symbol_isDefinedInCurrentRun
<pre><code class="language-scala" >def Symbol_isDefinedInCurrentRun(self: Symbol)(implicit ctx: Context): Boolean</pre></code>

### Symbol_isLocalDummy
<pre><code class="language-scala" >def Symbol_isLocalDummy(self: Symbol)(implicit ctx: Context): Boolean</pre></code>

### Symbol_isRefinementClass
<pre><code class="language-scala" >def Symbol_isRefinementClass(self: Symbol)(implicit ctx: Context): Boolean</pre></code>

### Symbol_localContext
<pre><code class="language-scala" >def Symbol_localContext(self: Symbol)(implicit ctx: Context): Context</pre></code>

### Symbol_name
<pre><code class="language-scala" >def Symbol_name(self: Symbol)(implicit ctx: Context): String</pre></code>
The name of this symbol.

### Symbol_owner
<pre><code class="language-scala" >def Symbol_owner(self: Symbol)(implicit ctx: Context): Symbol</pre></code>
Owner of this symbol. The owner is the symbol in which this symbol is defined.

### Symbol_pos
<pre><code class="language-scala" >def Symbol_pos(self: Symbol)(implicit ctx: Context): Position</pre></code>
The position of this symbol

### Symbol_privateWithin
<pre><code class="language-scala" >def Symbol_privateWithin(self: Symbol)(implicit ctx: Context): Option[Type]</pre></code>
This symbol is private within the resulting type.

### Symbol_protectedWithin
<pre><code class="language-scala" >def Symbol_protectedWithin(self: Symbol)(implicit ctx: Context): Option[Type]</pre></code>
This symbol is protected within the resulting type.

### TermRef_apply
<pre><code class="language-scala" >def TermRef_apply(qual: TypeOrBounds, name: String)(implicit ctx: Context): TermRef</pre></code>

### TermRef_name
<pre><code class="language-scala" >def TermRef_name(self: TermRef)(implicit ctx: Context): String</pre></code>

### TermRef_qualifier
<pre><code class="language-scala" >def TermRef_qualifier(self: TermRef)(implicit ctx: Context): TypeOrBounds</pre></code>

### Term_pos
<pre><code class="language-scala" >def Term_pos(self: Term)(implicit ctx: Context): Position</pre></code>

### Term_tpe
<pre><code class="language-scala" >def Term_tpe(self: Term)(implicit ctx: Context): Type</pre></code>

### Term_underlying
<pre><code class="language-scala" >def Term_underlying(self: Term)(implicit ctx: Context): Term</pre></code>

### Term_underlyingArgument
<pre><code class="language-scala" >def Term_underlyingArgument(self: Term)(implicit ctx: Context): Term</pre></code>

### ThisType_tref
<pre><code class="language-scala" >def ThisType_tref(self: ThisType)(implicit ctx: Context): Type</pre></code>

### This_apply
<pre><code class="language-scala" >def This_apply(cls: ClassDefSymbol)(implicit ctx: Context): This</pre></code>

### This_copy
<pre><code class="language-scala" >def This_copy(original: Tree)(qual: Option[Id])(implicit ctx: Context): This</pre></code>

### This_id
<pre><code class="language-scala" >def This_id(self: This)(implicit ctx: Context): Option[Id]</pre></code>

### Tree_pos
<pre><code class="language-scala" >def Tree_pos(self: Tree)(implicit ctx: Context): Position</pre></code>

### Tree_symbol
<pre><code class="language-scala" >def Tree_symbol(self: Tree)(implicit ctx: Context): Symbol</pre></code>

### Try_apply
<pre><code class="language-scala" >def Try_apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Try</pre></code>

### Try_body
<pre><code class="language-scala" >def Try_body(self: Try)(implicit ctx: Context): Term</pre></code>

### Try_cases
<pre><code class="language-scala" >def Try_cases(self: Try)(implicit ctx: Context): List[CaseDef]</pre></code>

### Try_copy
<pre><code class="language-scala" >def Try_copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Try</pre></code>

### Try_finalizer
<pre><code class="language-scala" >def Try_finalizer(self: Try)(implicit ctx: Context): Option[Term]</pre></code>

### TypeApply_apply
<pre><code class="language-scala" >def TypeApply_apply(fn: Term, args: List[TypeTree])(implicit ctx: Context): TypeApply</pre></code>

### TypeApply_args
<pre><code class="language-scala" >def TypeApply_args(self: TypeApply)(implicit ctx: Context): List[TypeTree]</pre></code>

### TypeApply_copy
<pre><code class="language-scala" >def TypeApply_copy(original: Tree)(fun: Term, args: List[TypeTree])(implicit ctx: Context): TypeApply</pre></code>

### TypeApply_fun
<pre><code class="language-scala" >def TypeApply_fun(self: TypeApply)(implicit ctx: Context): Term</pre></code>

### TypeBindSymbol_tree
<pre><code class="language-scala" >def TypeBindSymbol_tree(self: TypeBindSymbol)(implicit ctx: Context): TypeBind</pre></code>
TypeBind pattern of this definition

### TypeBind_body
<pre><code class="language-scala" >def TypeBind_body(self: TypeBind)(implicit ctx: Context): Tree</pre></code>

### TypeBind_copy
<pre><code class="language-scala" >def TypeBind_copy(original: TypeBind)(name: String, tpt: Tree)(implicit ctx: Context): TypeBind</pre></code>

### TypeBind_name
<pre><code class="language-scala" >def TypeBind_name(self: TypeBind)(implicit ctx: Context): String</pre></code>

### TypeBlock_aliases
<pre><code class="language-scala" >def TypeBlock_aliases(self: TypeBlock)(implicit ctx: Context): List[TypeDef]</pre></code>

### TypeBlock_apply
<pre><code class="language-scala" >def TypeBlock_apply(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock</pre></code>

### TypeBlock_copy
<pre><code class="language-scala" >def TypeBlock_copy(original: TypeBlock)(aliases: List[TypeDef], tpt: TypeTree)(implicit ctx: Context): TypeBlock</pre></code>

### TypeBlock_tpt
<pre><code class="language-scala" >def TypeBlock_tpt(self: TypeBlock)(implicit ctx: Context): TypeTree</pre></code>

### TypeBoundsTree_hi
<pre><code class="language-scala" >def TypeBoundsTree_hi(self: TypeBoundsTree)(implicit ctx: Context): TypeTree</pre></code>

### TypeBoundsTree_low
<pre><code class="language-scala" >def TypeBoundsTree_low(self: TypeBoundsTree)(implicit ctx: Context): TypeTree</pre></code>

### TypeBoundsTree_tpe
<pre><code class="language-scala" >def TypeBoundsTree_tpe(self: TypeBoundsTree)(implicit ctx: Context): TypeBounds</pre></code>

### TypeBounds_hi
<pre><code class="language-scala" >def TypeBounds_hi(self: TypeBounds)(implicit ctx: Context): Type</pre></code>

### TypeBounds_low
<pre><code class="language-scala" >def TypeBounds_low(self: TypeBounds)(implicit ctx: Context): Type</pre></code>

### TypeCaseDef_module_apply
<pre><code class="language-scala" >def TypeCaseDef_module_apply(pattern: TypeTree, body: TypeTree)(implicit ctx: Context): TypeCaseDef</pre></code>

### TypeCaseDef_module_copy
<pre><code class="language-scala" >def TypeCaseDef_module_copy(original: TypeCaseDef)(pattern: TypeTree, body: TypeTree)(implicit ctx: Context): TypeCaseDef</pre></code>

### TypeCaseDef_pattern
<pre><code class="language-scala" >def TypeCaseDef_pattern(self: TypeCaseDef)(implicit ctx: Context): TypeTree</pre></code>

### TypeCaseDef_rhs
<pre><code class="language-scala" >def TypeCaseDef_rhs(self: TypeCaseDef)(implicit ctx: Context): TypeTree</pre></code>

### TypeDefSymbol_isTypeParam
<pre><code class="language-scala" >def TypeDefSymbol_isTypeParam(self: TypeDefSymbol)(implicit ctx: Context): Boolean</pre></code>

### TypeDefSymbol_tree
<pre><code class="language-scala" >def TypeDefSymbol_tree(self: TypeDefSymbol)(implicit ctx: Context): TypeDef</pre></code>
TypeDef tree of this definition

### TypeDef_apply
<pre><code class="language-scala" >def TypeDef_apply(symbol: TypeDefSymbol)(implicit ctx: Context): TypeDef</pre></code>

### TypeDef_copy
<pre><code class="language-scala" >def TypeDef_copy(original: TypeDef)(name: String, rhs: Tree)(implicit ctx: Context): TypeDef</pre></code>

### TypeDef_rhs
<pre><code class="language-scala" >def TypeDef_rhs(self: TypeDef)(implicit ctx: Context): Tree</pre></code>

### TypeDef_symbol
<pre><code class="language-scala" >def TypeDef_symbol(self: TypeDef)(implicit ctx: Context): TypeDefSymbol</pre></code>

### TypeIdent_copy
<pre><code class="language-scala" >def TypeIdent_copy(original: TypeIdent)(name: String)(implicit ctx: Context): TypeIdent</pre></code>

### TypeIdent_name
<pre><code class="language-scala" >def TypeIdent_name(self: TypeIdent)(implicit ctx: Context): String</pre></code>

### TypeLambda_paramBounds
<pre><code class="language-scala" >def TypeLambda_paramBounds(self: TypeLambda)(implicit ctx: Context): List[TypeBounds]</pre></code>

### TypeLambda_paramNames
<pre><code class="language-scala" >def TypeLambda_paramNames(self: TypeLambda)(implicit ctx: Context): List[String]</pre></code>

### TypeLambda_resType
<pre><code class="language-scala" >def TypeLambda_resType(self: TypeLambda)(implicit ctx: Context): Type</pre></code>

### TypeRef_name
<pre><code class="language-scala" >def TypeRef_name(self: TypeRef)(implicit ctx: Context): String</pre></code>

### TypeRef_qualifier
<pre><code class="language-scala" >def TypeRef_qualifier(self: TypeRef)(implicit ctx: Context): TypeOrBounds</pre></code>

### TypeSelect_apply
<pre><code class="language-scala" >def TypeSelect_apply(qualifier: Term, name: String)(implicit ctx: Context): TypeSelect</pre></code>

### TypeSelect_copy
<pre><code class="language-scala" >def TypeSelect_copy(original: TypeSelect)(qualifier: Term, name: String)(implicit ctx: Context): TypeSelect</pre></code>

### TypeSelect_name
<pre><code class="language-scala" >def TypeSelect_name(self: TypeSelect)(implicit ctx: Context): String</pre></code>

### TypeSelect_qualifier
<pre><code class="language-scala" >def TypeSelect_qualifier(self: TypeSelect)(implicit ctx: Context): Term</pre></code>

### TypeTree_pos
<pre><code class="language-scala" >def TypeTree_pos(self: TypeTree)(implicit ctx: Context): Position</pre></code>

### TypeTree_symbol
<pre><code class="language-scala" >def TypeTree_symbol(self: TypeTree)(implicit ctx: Context): Symbol</pre></code>

### TypeTree_tpe
<pre><code class="language-scala" >def TypeTree_tpe(self: TypeTree)(implicit ctx: Context): Type</pre></code>

### Type_$u003C:$u003C
<pre><code class="language-scala" >def Type_$u003C:$u003C(self: Type)(that: Type)(implicit ctx: Context): Boolean</pre></code>

### Type_=:=
<pre><code class="language-scala" >def Type_=:=(self: Type)(that: Type)(implicit ctx: Context): Boolean</pre></code>

### Type_classSymbol
<pre><code class="language-scala" >def Type_classSymbol(self: Type)(implicit ctx: Context): Option[ClassDefSymbol]</pre></code>

### Type_dealias
<pre><code class="language-scala" >def Type_dealias(self: Type)(implicit ctx: Context): Type</pre></code>
Follow aliases and dereferences LazyRefs, annotated types and instantiated
TypeVars until type is no longer alias type, annotated type, LazyRef,
or instantiated type variable.

### Type_derivesFrom
<pre><code class="language-scala" >def Type_derivesFrom(self: Type)(cls: ClassDefSymbol)(implicit ctx: Context): Boolean</pre></code>
Is this type an instance of a non-bottom subclass of the given class `cls`?

### Type_isDependentFunctionType
<pre><code class="language-scala" >def Type_isDependentFunctionType(self: Type)(implicit ctx: Context): Boolean</pre></code>
Is this type a dependent function type?

***see*** `Type_isFunctionType`

### Type_isErasedFunctionType
<pre><code class="language-scala" >def Type_isErasedFunctionType(self: Type)(implicit ctx: Context): Boolean</pre></code>
Is this type an erased function type?

***see*** `Type_isFunctionType`

### Type_isFunctionType
<pre><code class="language-scala" >def Type_isFunctionType(self: Type)(implicit ctx: Context): Boolean</pre></code>
Is this type a function type?

***return*** true if the dealised type of `self` without refinement is `FunctionN[T1, T2, ..., Tn]`

***Note*** The function
* returns true for `given Int => Int` and `erased Int => Int`
* returns false for `List[Int]`, despite that `List[Int] <:< Int => Int`.


### Type_isImplicitFunctionType
<pre><code class="language-scala" >def Type_isImplicitFunctionType(self: Type)(implicit ctx: Context): Boolean</pre></code>
Is this type an implicit function type?

***see*** `Type_isFunctionType`

### Type_isSingleton
<pre><code class="language-scala" >def Type_isSingleton(self: Type)(implicit ctx: Context): Boolean</pre></code>

### Type_memberType
<pre><code class="language-scala" >def Type_memberType(self: Type)(member: Symbol)(implicit ctx: Context): Type</pre></code>

### Type_typeSymbol
<pre><code class="language-scala" >def Type_typeSymbol(self: Type)(implicit ctx: Context): Symbol</pre></code>

### Type_widen
<pre><code class="language-scala" >def Type_widen(self: Type)(implicit ctx: Context): Type</pre></code>
Widen from singleton type to its underlying non-singleton
base type by applying one or more `underlying` dereferences,
Also go from => T to T.
Identity for all other types. Example:
class Outer { class C ; val x: C }
def o: Outer
<o.x.type>.widen = o.C

### Typed_apply
<pre><code class="language-scala" >def Typed_apply(expr: Term, tpt: TypeTree)(implicit ctx: Context): Typed</pre></code>

### Typed_copy
<pre><code class="language-scala" >def Typed_copy(original: Tree)(expr: Term, tpt: TypeTree)(implicit ctx: Context): Typed</pre></code>

### Typed_expr
<pre><code class="language-scala" >def Typed_expr(self: Typed)(implicit ctx: Context): Term</pre></code>

### Typed_tpt
<pre><code class="language-scala" >def Typed_tpt(self: Typed)(implicit ctx: Context): TypeTree</pre></code>

### ValDefSymbol_companionClass
<pre><code class="language-scala" >def ValDefSymbol_companionClass(self: ValDefSymbol)(implicit ctx: Context): Option[ClassDefSymbol]</pre></code>

### ValDefSymbol_moduleClass
<pre><code class="language-scala" >def ValDefSymbol_moduleClass(self: ValDefSymbol)(implicit ctx: Context): Option[ClassDefSymbol]</pre></code>
The class symbol of the companion module class

### ValDefSymbol_tree
<pre><code class="language-scala" >def ValDefSymbol_tree(self: ValDefSymbol)(implicit ctx: Context): ValDef</pre></code>
ValDef tree of this definition

### ValDef_apply
<pre><code class="language-scala" >def ValDef_apply(symbol: ValDefSymbol, rhs: Option[Term])(implicit ctx: Context): ValDef</pre></code>

### ValDef_copy
<pre><code class="language-scala" >def ValDef_copy(original: ValDef)(name: String, tpt: TypeTree, rhs: Option[Term])(implicit ctx: Context): ValDef</pre></code>

### ValDef_rhs
<pre><code class="language-scala" >def ValDef_rhs(self: ValDef)(implicit ctx: Context): Option[Term]</pre></code>

### ValDef_symbol
<pre><code class="language-scala" >def ValDef_symbol(self: ValDef)(implicit ctx: Context): ValDefSymbol</pre></code>

### ValDef_tpt
<pre><code class="language-scala" >def ValDef_tpt(self: ValDef)(implicit ctx: Context): TypeTree</pre></code>

### While_apply
<pre><code class="language-scala" >def While_apply(cond: Term, body: Term)(implicit ctx: Context): While</pre></code>

### While_body
<pre><code class="language-scala" >def While_body(self: While)(implicit ctx: Context): Term</pre></code>

### While_cond
<pre><code class="language-scala" >def While_cond(self: While)(implicit ctx: Context): Term</pre></code>

### While_copy
<pre><code class="language-scala" >def While_copy(original: Tree)(cond: Term, body: Term)(implicit ctx: Context): While</pre></code>

### WildcardTypeTree_tpe
<pre><code class="language-scala" >def WildcardTypeTree_tpe(self: WildcardTypeTree)(implicit ctx: Context): TypeOrBounds</pre></code>

### error
<pre><code class="language-scala" >def error(msg: => String, source: SourceFile, start: Int, end: Int)(implicit ctx: Context): Unit</pre></code>
Report a compilation error with the given message at the given position range

### error
<pre><code class="language-scala" >def error(msg: => String, pos: Position)(implicit ctx: Context): Unit</pre></code>
Report a compilation error with the given message at the given position

### matchAndType
<pre><code class="language-scala" >def matchAndType(tpe: TypeOrBounds)(implicit ctx: Context): Option[AndType]</pre></code>

### matchAnnotated
<pre><code class="language-scala" >def matchAnnotated(tree: Tree)(implicit ctx: Context): Option[Annotated]</pre></code>

### matchAnnotatedType
<pre><code class="language-scala" >def matchAnnotatedType(tpe: TypeOrBounds)(implicit ctx: Context): Option[AnnotatedType]</pre></code>

### matchApplied
<pre><code class="language-scala" >def matchApplied(tree: Tree)(implicit ctx: Context): Option[Applied]</pre></code>

### matchAppliedType
<pre><code class="language-scala" >def matchAppliedType(tpe: TypeOrBounds)(implicit ctx: Context): Option[AppliedType]</pre></code>

### matchApply
<pre><code class="language-scala" >def matchApply(tree: Tree)(implicit ctx: Context): Option[Apply]</pre></code>

### matchAssign
<pre><code class="language-scala" >def matchAssign(tree: Tree)(implicit ctx: Context): Option[Assign]</pre></code>

### matchBindSymbol
<pre><code class="language-scala" >def matchBindSymbol(symbol: Symbol)(implicit ctx: Context): Option[BindSymbol]</pre></code>

### matchBlock
<pre><code class="language-scala" >def matchBlock(tree: Tree)(implicit ctx: Context): Option[Block]</pre></code>

### matchByName
<pre><code class="language-scala" >def matchByName(tree: Tree)(implicit ctx: Context): Option[ByName]</pre></code>

### matchByNameType
<pre><code class="language-scala" >def matchByNameType(tpe: TypeOrBounds)(implicit ctx: Context): Option[ByNameType]</pre></code>

### matchCaseDef
<pre><code class="language-scala" >def matchCaseDef(tree: Tree)(implicit ctx: Context): Option[CaseDef]</pre></code>

### matchClassDef
<pre><code class="language-scala" >def matchClassDef(tree: Tree)(implicit ctx: Context): Option[ClassDef]</pre></code>

### matchClassDefSymbol
<pre><code class="language-scala" >def matchClassDefSymbol(symbol: Symbol)(implicit ctx: Context): Option[ClassDefSymbol]</pre></code>

### matchConstantType
<pre><code class="language-scala" >def matchConstantType(tpe: TypeOrBounds)(implicit ctx: Context): Option[ConstantType]</pre></code>

### matchConstant_Boolean
<pre><code class="language-scala" >def matchConstant_Boolean(constant: Constant): Option[Boolean]</pre></code>

### matchConstant_Byte
<pre><code class="language-scala" >def matchConstant_Byte(constant: Constant): Option[Byte]</pre></code>

### matchConstant_Char
<pre><code class="language-scala" >def matchConstant_Char(constant: Constant): Option[Char]</pre></code>

### matchConstant_ClassTag
<pre><code class="language-scala" >def matchConstant_ClassTag(constant: Constant): Option[Type]</pre></code>

### matchConstant_Double
<pre><code class="language-scala" >def matchConstant_Double(constant: Constant): Option[Double]</pre></code>

### matchConstant_Float
<pre><code class="language-scala" >def matchConstant_Float(constant: Constant): Option[Float]</pre></code>

### matchConstant_Int
<pre><code class="language-scala" >def matchConstant_Int(constant: Constant): Option[Int]</pre></code>

### matchConstant_Long
<pre><code class="language-scala" >def matchConstant_Long(constant: Constant): Option[Long]</pre></code>

### matchConstant_Null
<pre><code class="language-scala" >def matchConstant_Null(constant: Constant): Boolean</pre></code>

### matchConstant_Short
<pre><code class="language-scala" >def matchConstant_Short(constant: Constant): Option[Short]</pre></code>

### matchConstant_String
<pre><code class="language-scala" >def matchConstant_String(constant: Constant): Option[String]</pre></code>

### matchConstant_Symbol
<pre><code class="language-scala" >def matchConstant_Symbol(constant: Constant): Option[Symbol]</pre></code>

### matchConstant_Unit
<pre><code class="language-scala" >def matchConstant_Unit(constant: Constant): Boolean</pre></code>

### matchDefDef
<pre><code class="language-scala" >def matchDefDef(tree: Tree)(implicit ctx: Context): Option[DefDef]</pre></code>

### matchDefDefSymbol
<pre><code class="language-scala" >def matchDefDefSymbol(symbol: Symbol)(implicit ctx: Context): Option[DefDefSymbol]</pre></code>

### matchDefinition
<pre><code class="language-scala" >def matchDefinition(tree: Tree)(implicit ctx: Context): Option[Definition]</pre></code>

### matchIdent
<pre><code class="language-scala" >def matchIdent(tree: Tree)(implicit ctx: Context): Option[Ident]</pre></code>

### matchIf
<pre><code class="language-scala" >def matchIf(tree: Tree)(implicit ctx: Context): Option[If]</pre></code>

### matchImplicitMatch
<pre><code class="language-scala" >def matchImplicitMatch(tree: Tree)(implicit ctx: Context): Option[ImpliedMatch]</pre></code>

### matchImport
<pre><code class="language-scala" >def matchImport(tree: Tree)(implicit ctx: Context): Option[Import]</pre></code>

### matchInferred
<pre><code class="language-scala" >def matchInferred(tree: Tree)(implicit ctx: Context): Option[Inferred]</pre></code>

### matchInlined
<pre><code class="language-scala" >def matchInlined(tree: Tree)(implicit ctx: Context): Option[Inlined]</pre></code>

### matchLambda
<pre><code class="language-scala" >def matchLambda(tree: Tree)(implicit ctx: Context): Option[Lambda]</pre></code>

### matchLambdaTypeTree
<pre><code class="language-scala" >def matchLambdaTypeTree(tree: Tree)(implicit ctx: Context): Option[LambdaTypeTree]</pre></code>

### matchLiteral
<pre><code class="language-scala" >def matchLiteral(tree: Tree)(implicit ctx: Context): Option[Literal]</pre></code>

### matchMatch
<pre><code class="language-scala" >def matchMatch(tree: Tree)(implicit ctx: Context): Option[Match]</pre></code>

### matchMatchType
<pre><code class="language-scala" >def matchMatchType(tpe: TypeOrBounds)(implicit ctx: Context): Option[MatchType]</pre></code>

### matchMatchTypeTree
<pre><code class="language-scala" >def matchMatchTypeTree(tree: Tree)(implicit ctx: Context): Option[MatchTypeTree]</pre></code>

### matchMethodType
<pre><code class="language-scala" >def matchMethodType(tpe: TypeOrBounds)(implicit ctx: Context): Option[MethodType]</pre></code>

### matchNamedArg
<pre><code class="language-scala" >def matchNamedArg(tree: Tree)(implicit ctx: Context): Option[NamedArg]</pre></code>

### matchNew
<pre><code class="language-scala" >def matchNew(tree: Tree)(implicit ctx: Context): Option[New]</pre></code>

### matchNoPrefix
<pre><code class="language-scala" >def matchNoPrefix(x: TypeOrBounds)(implicit ctx: Context): Option[NoPrefix]</pre></code>

### matchNoSymbol
<pre><code class="language-scala" >def matchNoSymbol(symbol: Symbol)(implicit ctx: Context): Boolean</pre></code>

### matchOmitSelector
<pre><code class="language-scala" >def matchOmitSelector(self: ImportSelector)(implicit ctx: Context): Option[OmitSelector]</pre></code>

### matchOrType
<pre><code class="language-scala" >def matchOrType(tpe: TypeOrBounds)(implicit ctx: Context): Option[OrType]</pre></code>

### matchPackageClause
<pre><code class="language-scala" >def matchPackageClause(tree: Tree)(implicit ctx: Context): Option[PackageClause]</pre></code>

### matchPackageDef
<pre><code class="language-scala" >def matchPackageDef(tree: Tree)(implicit ctx: Context): Option[PackageDef]</pre></code>

### matchPackageDefSymbol
<pre><code class="language-scala" >def matchPackageDefSymbol(symbol: Symbol)(implicit ctx: Context): Option[PackageDefSymbol]</pre></code>

### matchParamRef
<pre><code class="language-scala" >def matchParamRef(tpe: TypeOrBounds)(implicit ctx: Context): Option[ParamRef]</pre></code>

### matchPattern_Alternatives
<pre><code class="language-scala" >def matchPattern_Alternatives(pattern: Pattern)(implicit ctx: Context): Option[Alternatives]</pre></code>

### matchPattern_Bind
<pre><code class="language-scala" >def matchPattern_Bind(x: Pattern)(implicit ctx: Context): Option[Bind]</pre></code>

### matchPattern_TypeTest
<pre><code class="language-scala" >def matchPattern_TypeTest(pattern: Pattern)(implicit ctx: Context): Option[TypeTest]</pre></code>

### matchPattern_Unapply
<pre><code class="language-scala" >def matchPattern_Unapply(pattern: Pattern)(implicit ctx: Context): Option[Unapply]</pre></code>

### matchPattern_Value
<pre><code class="language-scala" >def matchPattern_Value(pattern: Pattern): Option[Value]</pre></code>

### matchPattern_WildcardPattern
<pre><code class="language-scala" >def matchPattern_WildcardPattern(pattern: Pattern)(implicit ctx: Context): Option[WildcardPattern]</pre></code>

### matchPolyType
<pre><code class="language-scala" >def matchPolyType(tpe: TypeOrBounds)(implicit ctx: Context): Option[PolyType]</pre></code>

### matchProjection
<pre><code class="language-scala" >def matchProjection(tree: Tree)(implicit ctx: Context): Option[Projection]</pre></code>

### matchRecursiveThis
<pre><code class="language-scala" >def matchRecursiveThis(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveThis]</pre></code>

### matchRecursiveType
<pre><code class="language-scala" >def matchRecursiveType(tpe: TypeOrBounds)(implicit ctx: Context): Option[RecursiveType]</pre></code>

### matchRef
<pre><code class="language-scala" >def matchRef(tree: Tree)(implicit ctx: Context): Option[Ref]</pre></code>

### matchRefined
<pre><code class="language-scala" >def matchRefined(tree: Tree)(implicit ctx: Context): Option[Refined]</pre></code>

### matchRefinement
<pre><code class="language-scala" >def matchRefinement(tpe: TypeOrBounds)(implicit ctx: Context): Option[Refinement]</pre></code>

### matchRenameSelector
<pre><code class="language-scala" >def matchRenameSelector(self: ImportSelector)(implicit ctx: Context): Option[RenameSelector]</pre></code>

### matchRepeated
<pre><code class="language-scala" >def matchRepeated(tree: Tree)(implicit ctx: Context): Option[Repeated]</pre></code>

### matchReturn
<pre><code class="language-scala" >def matchReturn(tree: Tree)(implicit ctx: Context): Option[Return]</pre></code>

### matchSelect
<pre><code class="language-scala" >def matchSelect(tree: Tree)(implicit ctx: Context): Option[Select]</pre></code>

### matchSelectOuter
<pre><code class="language-scala" >def matchSelectOuter(tree: Tree)(implicit ctx: Context): Option[SelectOuter]</pre></code>

### matchSimpleSelector
<pre><code class="language-scala" >def matchSimpleSelector(self: ImportSelector)(implicit ctx: Context): Option[SimpleSelector]</pre></code>

### matchSingleton
<pre><code class="language-scala" >def matchSingleton(tree: Tree)(implicit ctx: Context): Option[Singleton]</pre></code>

### matchStatement
<pre><code class="language-scala" >def matchStatement(tree: Tree)(implicit ctx: Context): Option[Statement]</pre></code>

### matchSuper
<pre><code class="language-scala" >def matchSuper(tree: Tree)(implicit ctx: Context): Option[Super]</pre></code>

### matchSuperType
<pre><code class="language-scala" >def matchSuperType(tpe: TypeOrBounds)(implicit ctx: Context): Option[SuperType]</pre></code>

### matchSymRef
<pre><code class="language-scala" >def matchSymRef(tpe: TypeOrBounds)(implicit ctx: Context): Option[SymRef]</pre></code>

### matchSymRef_unapply
<pre><code class="language-scala" >def matchSymRef_unapply(tpe: TypeOrBounds)(implicit ctx: Context): Option[(Symbol, TypeOrBounds)]</pre></code>

### matchTerm
<pre><code class="language-scala" >def matchTerm(tree: Tree)(implicit ctx: Context): Option[Term]</pre></code>

### matchTermRef
<pre><code class="language-scala" >def matchTermRef(tpe: TypeOrBounds)(implicit ctx: Context): Option[TermRef]</pre></code>

### matchTermSymbol
<pre><code class="language-scala" >def matchTermSymbol(symbol: Symbol)(implicit ctx: Context): Option[TermSymbol]</pre></code>

### matchThis
<pre><code class="language-scala" >def matchThis(tree: Tree)(implicit ctx: Context): Option[This]</pre></code>

### matchThisType
<pre><code class="language-scala" >def matchThisType(tpe: TypeOrBounds)(implicit ctx: Context): Option[ThisType]</pre></code>

### matchTry
<pre><code class="language-scala" >def matchTry(tree: Tree)(implicit ctx: Context): Option[Try]</pre></code>

### matchType
<pre><code class="language-scala" >def matchType(x: TypeOrBounds)(implicit ctx: Context): Option[Type]</pre></code>

### matchTypeApply
<pre><code class="language-scala" >def matchTypeApply(tree: Tree)(implicit ctx: Context): Option[TypeApply]</pre></code>

### matchTypeBind
<pre><code class="language-scala" >def matchTypeBind(tree: Tree)(implicit ctx: Context): Option[TypeBind]</pre></code>

### matchTypeBindSymbol
<pre><code class="language-scala" >def matchTypeBindSymbol(symbol: Symbol)(implicit ctx: Context): Option[TypeBindSymbol]</pre></code>

### matchTypeBlock
<pre><code class="language-scala" >def matchTypeBlock(tree: Tree)(implicit ctx: Context): Option[TypeBlock]</pre></code>

### matchTypeBounds
<pre><code class="language-scala" >def matchTypeBounds(x: TypeOrBounds)(implicit ctx: Context): Option[TypeBounds]</pre></code>

### matchTypeBoundsTree
<pre><code class="language-scala" >def matchTypeBoundsTree(tree: Tree)(implicit ctx: Context): Option[TypeBoundsTree]</pre></code>

### matchTypeCaseDef
<pre><code class="language-scala" >def matchTypeCaseDef(tree: Tree)(implicit ctx: Context): Option[TypeCaseDef]</pre></code>

### matchTypeDef
<pre><code class="language-scala" >def matchTypeDef(tree: Tree)(implicit ctx: Context): Option[TypeDef]</pre></code>

### matchTypeDefSymbol
<pre><code class="language-scala" >def matchTypeDefSymbol(symbol: Symbol)(implicit ctx: Context): Option[TypeDefSymbol]</pre></code>

### matchTypeIdent
<pre><code class="language-scala" >def matchTypeIdent(tree: Tree)(implicit ctx: Context): Option[TypeIdent]</pre></code>

### matchTypeLambda
<pre><code class="language-scala" >def matchTypeLambda(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeLambda]</pre></code>

### matchTypeRef
<pre><code class="language-scala" >def matchTypeRef(tpe: TypeOrBounds)(implicit ctx: Context): Option[TypeRef]</pre></code>

### matchTypeSelect
<pre><code class="language-scala" >def matchTypeSelect(tree: Tree)(implicit ctx: Context): Option[TypeSelect]</pre></code>

### matchTypeSymbol
<pre><code class="language-scala" >def matchTypeSymbol(symbol: Symbol)(implicit ctx: Context): Option[TypeSymbol]</pre></code>

### matchTypeTree
<pre><code class="language-scala" >def matchTypeTree(tree: Tree)(implicit ctx: Context): Option[TypeTree]</pre></code>

### matchTyped
<pre><code class="language-scala" >def matchTyped(tree: Tree)(implicit ctx: Context): Option[Typed]</pre></code>

### matchValDef
<pre><code class="language-scala" >def matchValDef(tree: Tree)(implicit ctx: Context): Option[ValDef]</pre></code>

### matchValDefSymbol
<pre><code class="language-scala" >def matchValDefSymbol(symbol: Symbol)(implicit ctx: Context): Option[ValDefSymbol]</pre></code>

### matchWhile
<pre><code class="language-scala" >def matchWhile(tree: Tree)(implicit ctx: Context): Option[While]</pre></code>

### matchWildcardTypeTree
<pre><code class="language-scala" >def matchWildcardTypeTree(tree: Tree)(implicit ctx: Context): Option[WildcardTypeTree]</pre></code>

### rootContext
<pre><code class="language-scala" >def rootContext: Context</pre></code>
Context of the macro expansion

### rootPosition
<pre><code class="language-scala" >def rootPosition: Position</pre></code>
Root position of this tasty context. For macros it corresponds to the expansion site.

### settings
<pre><code class="language-scala" >def settings: Settings</pre></code>

### typeChecks
<pre><code class="language-scala" >def typeChecks(code: String)(implicit ctx: Context): Boolean</pre></code>
Whether the code type checks in the given context?

***code*** The code to be type checked
The code should be a sequence of expressions or statements that may appear in a block.

### warning
<pre><code class="language-scala" >def warning(msg: => String, source: SourceFile, start: Int, end: Int)(implicit ctx: Context): Unit</pre></code>
Report a compilation warning with the given message at the given position range

### warning
<pre><code class="language-scala" >def warning(msg: => String, pos: Position)(implicit ctx: Context): Unit</pre></code>
Report a compilation warning with the given message at the given position

