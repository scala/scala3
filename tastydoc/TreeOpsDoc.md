# Tasty Reflect Documentation
This is an attempt at documenting some of the Tasty Reflect API for an easier understanding.

Information mostly come from `compiler/src/dotty/tools/dotc/tastyreflect/TreeOpsImpl.scala`, `library/src/scala/tasty/reflect/TreeOps.scala`, `compiler/src/dotty/tools/dotc/tastyreflect/TypeOrBoundsTreesOpsImpl.scala`

It follows the structure of `library/src/scala/tasty/reflect/Core.scala`

## Tree
### PackageClause
Represent a new package
* `def pid: Term.Ref` Name of the package
* `def stats: List[Tree]` "Body" of the package
### Import
Represent an import
* `impliedOnly: Boolean`
* `expr: Term` What to import
* `selectors: List[ImportSelector]` Package "path"
### Statement
#### Definition
##### PackageDef
##### ClassDef
Represent a class (or an object?)
* `name: String` Name of the class
* `constr: DefDef` Constructor
* `parents: List[TermOrTypeTree]`
* `derived: List[TypeTree]` //Classes dervied from this one
* `selfOpt: Option[ValDef]` //Self paramaters
* `body: List[Statement]` Body of the class
##### TypeDef
Represent a type definition or a generic type
* `name: String` Identifier
* `rhs: TypeOrBoundsTree`
##### DefDef
Represent a function
* `name: String` Name of the function

  Specific value:
  * `<init>` Used for constructors
* `typeParams: List[TypeDef]`
* `paramss: List[List[ValDef]]` Arguments of the function
* `tpt: TypeTree` Return type of the function
* `rhs: Option[Term]` Body of the function
##### ValDef
Represent a value
* `name: String` Name
* `tpt: TypeTree` Type
* `rhs: Option[Term]` Body

## TypeOrBoundsTree
### TypeTree
#### Inferred
#### Ident
Used to represent simple type such as Int, String
#### Select
#### Project
#### Singleton
#### Refined
#### Applied
<!-- For type of the form `x[y]`
* `tycon: Type` x
* `args: List[Type]` y -->
#### Annotated
#### MatchType
#### ByName
#### LambdaTypeTree
#### TypeBind
#### TypeBlock

## TypeOrBounds
### Type
#### ConstantType
#### SymRef
<!-- ..... -->
#### AppliedType
For type of the form `x[y]
`tycon: Type` x
`args: List[TypeOrBounds /* Type | TypeBounds */]` y