package scala.quoted.meta

import scala.annotation.experimental

trait Meta {
  val internal: Meta.Internal
}
object Meta {

  trait Internal {

    val position: PositionAPI

    val sourceFile: SourceFileAPI

    val signature: SignatureAPI

    val paramClause: ParamClauseAPI
    val termParamClause: TermParamClauseAPI
    val typeParamClause: TypeParamClauseAPI

    val flags: FlagsAPI

    val symbol: SymbolAPI

    val tree: TreeAPI
    val packageClause: PackageClauseAPI
    val statement: StatementAPI
    val importOrExport: ImportOrExportAPI
    val `import`: ImportAPI
    val `export`: ExportAPI
    val definition: DefinitionAPI
    val classDef: ClassDefAPI
    val typeDef: TypeDefAPI
    val valOrDefDef: ValOrDefDefAPI
    val defDef: DefDefAPI
    val valDef: ValDefAPI
    val term: TermAPI
    val ref: RefAPI
    val ident: IdentAPI
    val wildcard: WildcardAPI
    val select: SelectAPI
    val literal: LiteralAPI
    val `this`: ThisAPI
    val `new`: NewAPI
    val namedArg: NamedArgAPI
    val apply: ApplyAPI
    val typeApply: TypeApplyAPI
    val `super`: SuperAPI
    val assign: AssignAPI
    val block: BlockAPI
    val closure: ClosureAPI
    val `if`: IfAPI
    val `match`: MatchAPI
    val summonFrom: SummonFromAPI
    val `try`: TryAPI
    val `return`: ReturnAPI
    val repeated: RepeatedAPI
    val inlined: InlinedAPI
    val selectOuter: SelectOuterAPI
    val `while`: WhileAPI
    val typed: TypedAPI
    val typedOrTest: TypedOrTestAPI
    val bind: BindAPI
    val unapply: UnapplyAPI
    val alternatives: AlternativesAPI
    val caseDef: CaseDefAPI
    val typeCaseDef: TypeCaseDefAPI
    val typeTree: TypeTreeAPI
    val inferred: InferredAPI
    val typeIdent: TypeIdentAPI
    val typeSelect: TypeSelectAPI
    val typeProjection: TypeProjectionAPI
    val singleton: SingletonAPI
    val refined: RefinedAPI
    val applied: AppliedAPI
    val annotated: AnnotatedAPI
    val matchTypeTree: MatchTypeTreeAPI
    val byName: ByNameAPI
    val lambdaTypeTree: LambdaTypeTreeAPI
    val typeBind: TypeBindAPI
    val typeBlock: TypeBlockAPI
    val typeBoundsTree: TypeBoundsTreeAPI
    val wildcardTypeTree: WildcardTypeTreeAPI

    val typeRepr: TypeReprAPI
    val namedType: NamedTypeAPI
    val termRef: TermRefAPI
    val typeRef: TypeRefAPI
    val constantType: ConstantTypeAPI
    val superType: SuperTypeAPI
    val refinement: RefinementAPI
    val appliedType: AppliedTypeAPI
    val annotatedType: AnnotatedTypeAPI
    val andOrType: AndOrTypeAPI
    val andType: AndTypeAPI
    val orType: OrTypeAPI
    val matchType: MatchTypeAPI
    val byNameType: ByNameTypeAPI
    val paramRef: ParamRefAPI
    val self: SelfAPI
    val thisType: ThisTypeAPI
    val recursiveThis: RecursiveThisAPI
    val recursiveType: RecursiveTypeAPI
    val lambdaType: LambdaTypeAPI
    val methodOrPoly: MethodOrPolyAPI
    val methodType: MethodTypeAPI
    val polyType: PolyTypeAPI
    val typeLambda: TypeLambdaAPI
    val matchCase: MatchCaseAPI
    val typeBounds: TypeBoundsAPI
    val noPrefix: NoPrefixAPI
    val flexibleType: FlexibleTypeAPI

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Position
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait PositionAPI {
    def ofMacroExpansion: Position
    def apply(sourceFile: SourceFile, start: Int, end: Int): Position
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      SourceFile
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait SourceFileAPI {
    def current: SourceFile
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Signature
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait SignatureAPI {}

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ParamClause
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait ParamClauseAPI {}

  trait TermParamClauseAPI {
    def apply(params: List[ValDef]): TermParamClause
  }

  trait TypeParamClauseAPI {
    def apply(params: List[ValDef]): TermParamClause
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Flags
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait FlagsAPI {

    /** Is this symbol `abstract` */
    def Abstract: Flags

    /**
      * Is this an abstract override method?
      *
      *  This corresponds to a definition declared as "abstract override def" in the source.
      *  See https://stackoverflow.com/questions/23645172/why-is-abstract-override-required-not-override-alone-in-subtrait for examples.
      */
    def AbsOverride: Flags

    /**
      * Is this generated by Scala compiler.
      *  Corresponds to ACC_SYNTHETIC in the JVM.
      */
    def Artifact: Flags

    /** Is this symbol `case` */
    def Case: Flags

    /** Is this symbol a getter for case class parameter */
    def CaseAccessor: Flags

    /** Is this symbol a type parameter marked as contravariant `-` */
    def Contravariant: Flags

    /** Is this symbol a type parameter marked as covariant `+` */
    def Covariant: Flags

    /** Is a declared, but not defined member */
    def Deferred: Flags

    /** The empty set of flags */
    def EmptyFlags: Flags

    /** Is this symbol an enum */
    def Enum: Flags

    /** Is this symbol `erased` */
    def Erased: Flags

    /** Is this symbol exported from provided instance */
    def Exported: Flags

    /** Is this symbol a `def` defined in an `extension` */
    def ExtensionMethod: Flags

    /** Is this symbol a getter or a setter */
    def FieldAccessor: Flags

    /** Is this symbol `final` */
    def Final: Flags

    /** Is this symbol an inferable ("given") parameter */
    def Given: Flags

    /** Is this symbol a parameter with a default value? */
    def HasDefault: Flags

    /** Is this symbol `implicit` */
    def Implicit: Flags

    /** Is an infix method or type */
    def Infix: Flags

    /** Is this symbol `inline` */
    def Inline: Flags

    /** Is this symbol invisible when typechecking? */
    def Invisible: Flags

    /** Is this symbol defined in a Java class */
    def JavaDefined: Flags

    /** Is implemented as a Java static */
    def JavaStatic: Flags

    /** Is this an annotation defined in Java */
    def JavaAnnotation: Flags

    /** Is this symbol `lazy` */
    def Lazy: Flags

    /** Is this symbol local? Used in conjunction with private/private[T] to mean private[this] extends Modifier protected[this] */
    def Local: Flags

    /** Is this symbol marked as a macro. An inline method containing top level splices */
    def Macro: Flags

    /** Is this symbol `def` */
    def Method: Flags

    /** Is this symbol an object or its class (used for a ValDef or a ClassDef extends Modifier respectively) */
    def Module: Flags

    /** Is this symbol a `var` (when used on a ValDef) */
    def Mutable: Flags

    /** Trait does not have fields or initialization code. */
    def NoInits: Flags

    /** Is this symbol `opaque` */
    def Opaque: Flags

    /** Is this symbol `open` */
    def Open: Flags

    /** Is this symbol `override` */
    def Override: Flags

    /** Is this symbol a package */
    def Package: Flags

    /** Is this symbol a parameter */
    def Param: Flags

    /** Is this symbol a parameter accessor */
    def ParamAccessor: Flags

    /** Is this symbol `private` */
    def Private: Flags

    /** Is this symbol labeled private[this] */
    def PrivateLocal: Flags

    /** Is this symbol `protected` */
    def Protected: Flags

    /** Was this symbol imported from Scala2.x */
    def Scala2x: Flags

    /** Is this symbol `sealed` */
    def Sealed: Flags

    /** Is this symbol member that is assumed to be stable and realizable */
    def StableRealizable: Flags

    /** Is this symbol to be tagged Java Synthetic */
    def Synthetic: Flags

    /** Is this symbol a trait */
    def Trait: Flags

    /** Is a transparent inline method or trait */
    def Transparent: Flags

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Symbol
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait SymbolAPI {

    /**
      * Symbol of the definition that encloses the current splicing context.
      *
      *  For example, the following call to `spliceOwner` would return the symbol `x`.
      *  ```scala sc:nocompile
      *  val x = ${ ... Symbol.spliceOwner ... }
      *  ```
      *
      *  For a macro splice, it is the symbol of the definition where the macro expansion happens.
      */
    def spliceOwner: Symbol

    /** Get package symbol if package is either defined in current compilation run or present on classpath. */
    def requiredPackage(path: String): Symbol

    /** Get class symbol if class is either defined in current compilation run or present on classpath. */
    def requiredClass(path: String): Symbol

    /** Get module symbol if module is either defined in current compilation run or present on classpath. */
    def requiredModule(path: String): Symbol

    /** Get method symbol if method is either defined in current compilation run or present on classpath. Throws if the method has an overload. */
    def requiredMethod(path: String): Symbol

    /** The class Symbol of a global class definition */
    def classSymbol(fullName: String): Symbol

    /**
      * Generates a new class symbol for a class with a public parameterless constructor.
      *  For more settings, look to the other newClass methods.
      *
      *  Example usage:
      *  ```
      *  val name: String = "myClass"
      *  val parents = List(TypeTree.of[Object], TypeTree.of[Foo])
      *  def decls(cls: Symbol): List[Symbol] =
      *    List(Symbol.newMethod(cls, "foo", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit])))
      *
      *  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = parents.map(_.tpe), decls, selfType = None)
      *  val fooSym = cls.declaredMethod("foo").head
      *
      *  val fooDef = DefDef(fooSym, argss => Some('{println(s"Calling foo")}.asTerm))
      *  val clsDef = ClassDef(cls, parents, body = List(fooDef))
      *  val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Foo])
      *
      *  Block(List(clsDef), newCls).asExprOf[Foo]
      *  ```
      *  constructs the equivalent to
      *   ```
      *  '{
      *    class myClass() extends Object with Foo {
      *      def foo(): Unit = println("Calling foo")
      *    }
      *    new myClass(): Foo
      *  }
      *  ```
      *
      *  @param owner The owner of the class
      *  @param name The name of the class
      *  @param parents The parent classes of the class. The first parent must not be a trait.
      *  @param decls The member declarations of the class provided the symbol of this class
      *  @param selfType The self type of the class if it has one
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the ClassDef constructor.
      *
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
    @experimental def newClass(owner: Symbol, name: String, parents: List[TypeRepr], decls: Symbol => List[Symbol], selfType: Option[TypeRepr]): Symbol

    /**
      * Generates a new class symbol for a class with a public single term clause constructor.
      *
      *  Example usage:
      *  ```
      *  val name = "myClass"
      *  def decls(cls: Symbol): List[Symbol] =
      *    List(Symbol.newMethod(cls, "foo", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit])))
      *  val parents = List(TypeTree.of[Object])
      *  val cls = Symbol.newClass(
      *    Symbol.spliceOwner,
      *    name,
      *    parents = _ => parents.map(_.tpe),
      *    decls,
      *    selfType = None,
      *    clsFlags = Flags.EmptyFlags,
      *    Symbol.noSymbol,
      *    List(("idx", TypeRepr.of[Int]), ("str", TypeRepr.of[String]))
      *  )
      *
      *  val fooSym = cls.declaredMethod("foo").head
      *  val idxSym = cls.fieldMember("idx")
      *  val strSym = cls.fieldMember("str")
      *  val fooDef = DefDef(fooSym, argss =>
      *    Some('{println(s"Foo method call with (${${Ref(idxSym).asExpr}}, ${${Ref(strSym).asExpr}})")}.asTerm)
      *  )
      *  val clsDef = ClassDef(cls, parents, body = List(fooDef))
      *  val newCls = Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), List('{0}.asTerm, '{string}.asTerm))
      *
      *  Block(List(clsDef), Apply(Select(newCls, cls.methodMember("foo")(0)), Nil)).asExprOf[Unit]
      *  ```
      *  construct the equivalent to
      *  ```
      *  '{
      *    class myClass(idx: Int, str: String) extends Object {
      *      def foo() =
      *        println(s"Foo method call with $idx, $str")
      *    }
      *    new myClass(0, "string").foo()
      *  }
      *  ```
      *  @param owner The owner of the class
      *  @param name The name of the class
      *  @param parents Function returning the parent classes of the class. The first parent must not be a trait.
      *  Takes the constructed class symbol as an argument. Calling `cls.typeRef.asType` as part of this function will lead to cyclic reference errors.
      *  @param clsFlags extra flags with which the class symbol should be constructed.
      *  @param clsPrivateWithin the symbol within which this new class symbol should be private. May be noSymbol.
      *  @param conParams constructor parameter pairs of names and types.
      *
      *  Parameters assigned by the constructor can be obtained via `classSymbol.memberField`.
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the ClassDef constructor.
      *
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
    @experimental def newClass(
        owner: Symbol,
        name: String,
        parents: Symbol => List[TypeRepr],
        decls: Symbol => List[Symbol],
        selfType: Option[TypeRepr],
        clsFlags: Flags,
        clsPrivateWithin: Symbol,
        conParams: List[(String, TypeRepr)],
    ): Symbol

    /**
      * Generates a new class symbol with a constructor of the shape signified by a passed PolyOrMethod parameter.
      *
      *  Example usage:
      *  ```
      *  val name = "myClass"
      *  def decls(cls: Symbol): List[Symbol] =
      *    List(Symbol.newMethod(cls, "getParam", MethodType(Nil)(_ => Nil, _ => cls.typeMember("T").typeRef)))
      *  val conMethodType =
      *    (classType: TypeRepr) => PolyType(List("T"))(_ => List(TypeBounds.empty), polyType =>
      *      MethodType(List("param"))((_: MethodType) => List(polyType.param(0)), (_: MethodType) =>
      *        AppliedType(classType, List(polyType.param(0)))
      *      )
      *    )
      *  val cls = Symbol.newClass(
      *    Symbol.spliceOwner,
      *    name,
      *    parents = _ => List(TypeRepr.of[Object]),
      *    decls,
      *    selfType = None,
      *    clsFlags = Flags.EmptyFlags,
      *    clsPrivateWithin = Symbol.noSymbol,
      *    clsAnnotations = Nil,
      *    conMethodType,
      *    conFlags = Flags.EmptyFlags,
      *    conPrivateWithin = Symbol.noSymbol,
      *    conParamFlags = List(List(Flags.EmptyFlags), List(Flags.EmptyFlags)),
      *    conParamPrivateWithins = List(List(Symbol.noSymbol), List(Symbol.noSymbol))
      *  )
      *
      *  val getParamSym = cls.declaredMethod("getParam").head
      *  def getParamRhs(): Option[Term] =
      *    val paramValue = This(cls).select(cls.fieldMember("param")).asExpr
      *    Some('{ println("Calling getParam"); $paramValue }.asTerm)
      *  val getParamDef = DefDef(getParamSym, _ => getParamRhs())
      *
      *  val clsDef = ClassDef(cls, List(TypeTree.of[Object]), body = List(getParamDef))
      *  val newCls =
      *    Apply(
      *      Select(
      *        Apply(
      *          TypeApply(Select(New(TypeIdent(cls)), cls.primaryConstructor), List(TypeTree.of[String])),
      *          List(Expr("test").asTerm)
      *        ),
      *        cls.methodMember("getParam").head
      *      ),
      *      Nil
      *    )
      *
      *  Block(List(clsDef), newCls).asExpr
      *  ```
      *  constructs the equivalent to
      *  ```
      *  '{
      *    class myClass[T](val param: T) extends Object {
      *      def getParam: T =
      *        println("Calling getParam")
      *        param
      *    }
      *    new myClass[String]("test").getParam()
      *  }
      *  ```
      *
      * @param owner The owner of the class
      * @param name The name of the class
      * @param parents Function returning the parent classes of the class. The first parent must not be a trait
      * Takes the constructed class symbol as an argument. Calling `cls.typeRef.asType` as part of this function will lead to cyclic reference errors.
      * @param decls The member declarations of the class provided the symbol of this class
      * @param selfType The self type of the class if it has one
      * @param clsFlags extra flags with which the class symbol should be constructed. Can be `Private` | `Protected` | `PrivateLocal` | `Local` | `Final` | `Trait` | `Abstract` | `Open`
      * @param clsPrivateWithin the symbol within which this new class symbol should be private. May be noSymbol
      * @param clsAnnotations annotations of the class
      * @param conMethodType Function returning MethodOrPoly type representing the type of the constructor.
      * Takes the result type as parameter which must be returned from the innermost MethodOrPoly and have type parameters applied if those are used.
      * PolyType may only represent the first clause of the constructor.
      * @param conFlags extra flags with which the constructor symbol should be constructed. Can be `Synthetic` | `Method` | `Private` | `Protected` | `PrivateLocal` | `Local`
      * @param conPrivateWithin the symbol within which the constructor for this new class symbol should be private. May be noSymbol.
      * @param conParamFlags extra flags with which the constructor parameter symbols should be constructed. Must match the shape of `conMethodType`.
      * For type parameters those can be `Param` | `Deferred` | `Private` | `PrivateLocal` | `Local`.
      * For term parameters those can be `ParamAccessor` | `Private` | `Protected` | `PrivateLocal` | `Local`
      * @param conParamPrivateWithins the symbols within which the constructor parameters should be private. Must match the shape of `conMethodType`. Can consist of noSymbol.
      *
      *  Term and type parameters assigned by the constructor can be obtained via `classSymbol.memberField`/`classSymbol.memberType`.
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the ClassDef constructor.
      *
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
    // Keep doc aligned with QuotesImpl's validFlags: `clsFlags` with `validClassFlags`, `conFlags` with `validClassConstructorFlags`,
    // conParamFlags with `validClassTypeParamFlags` and `validClassTermParamFlags`
    @experimental def newClass(
        owner: Symbol,
        name: String,
        parents: Symbol => List[TypeRepr],
        decls: Symbol => List[Symbol],
        selfType: Option[TypeRepr],
        clsFlags: Flags,
        clsPrivateWithin: Symbol,
        clsAnnotations: List[Term],
        conMethodType: TypeRepr => MethodOrPoly,
        conFlags: Flags,
        conPrivateWithin: Symbol,
        conParamFlags: List[List[Flags]],
        conParamPrivateWithins: List[List[Symbol]],
    ): Symbol

    /**
      * Generates a new module symbol with an associated module class symbol,
      *  this is equivalent to an `object` declaration in source code.
      *  This method returns the module symbol. The module class can be accessed calling `moduleClass` on this symbol.
      *
      *  Example usage:
      *  ```scala
      *  //{
      *  given Quotes = ???
      *  import quotes.reflect.*
      *  //}
      *  val moduleName: String = Symbol.freshName("MyModule")
      *  val parents = List(TypeTree.of[Object])
      *  def decls(cls: Symbol): List[Symbol] =
      *    List(Symbol.newMethod(cls, "run", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]), Flags.EmptyFlags, Symbol.noSymbol))
      *
      *  val mod = Symbol.newModule(Symbol.spliceOwner, moduleName, Flags.EmptyFlags, Flags.EmptyFlags, _ => parents.map(_.tpe), decls, Symbol.noSymbol)
      *  val cls = mod.moduleClass
      *  val runSym = cls.declaredMethod("run").head
      *
      *  val runDef = DefDef(runSym, _ => Some('{ println("run") }.asTerm))
      *  val modDef = ClassDef.module(mod, parents, body = List(runDef))
      *
      *  val callRun = Apply(Select(Ref(mod), runSym), Nil)
      *
      *  Block(modDef.toList, callRun)
      *  ```
      *  constructs the equivalent to
      *  ```scala
      *  //{
      *  given Quotes = ???
      *  import quotes.reflect.*
      *  //}
      *  '{
      *    object MyModule$macro$1 extends Object:
      *      def run(): Unit = println("run")
      *    MyModule$macro$1.run()
      *  }
      *  ```
      *
      *  @param parent The owner of the class
      *  @param name The name of the class
      *  @param modFlags extra flags with which the module symbol should be constructed
      *  @param clsFlags extra flags with which the module class symbol should be constructed
      *  @param parents A function that takes the symbol of the module class as input and returns the parent classes of the class. The first parent must not be a trait.
      *  @param decls A function that takes the symbol of the module class as input and return the symbols of its declared members
      *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to `ClassDef.module`.
      *
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      *
      *  @syntax markdown
      */
    @experimental def newModule(owner: Symbol, name: String, modFlags: Flags, clsFlags: Flags, parents: Symbol => List[TypeRepr], decls: Symbol => List[Symbol], privateWithin: Symbol): Symbol

    /**
      * Generates a new method symbol with the given parent, name and type.
      *
      *  To define a member method of a class, use the `newMethod` within the `decls` function of `newClass`.
      *
      *  @param parent The owner of the method
      *  @param name The name of the method
      *  @param tpe The type of the method (MethodType, PolyType, ByNameType)
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the DefDef constructor.
      *
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
    def newMethod(parent: Symbol, name: String, tpe: TypeRepr): Symbol

    /**
      * Works as the other newMethod, but with additional parameters.
      *
      *  To define a member method of a class, use the `newMethod` within the `decls` function of `newClass`.
      *
      *  @param parent The owner of the method
      *  @param name The name of the method
      *  @param tpe The type of the method (MethodType, PolyType, ByNameType)
      *  @param flags extra flags to with which the symbol should be constructed. `Method` flag will be added. Can be `Private | Protected | Override | Deferred | Final | Method | Implicit | Given | Local | JavaStatic | Synthetic | Artifact`
      *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
      */
    // Keep: `flags` doc aligned with QuotesImpl's `validMethodFlags`
    def newMethod(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol

    /**
      * Generates a new val/var/lazy val symbol with the given parent, name and type.
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the ValDef constructor.
      *
      *  Note: Also see ValDef.let
      *
      *  @param parent The owner of the val/var/lazy val
      *  @param name The name of the val/var/lazy val
      *  @param tpe The type of the val/var/lazy val
      *  @param flags extra flags to with which the symbol should be constructed. Can be `Private | Protected | Override | Deferred | Final | Param | Implicit | Lazy | Mutable | Local | ParamAccessor | Module | Package | Case | CaseAccessor | Given | Enum | JavaStatic | Synthetic | Artifact`
      *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
    // Keep: `flags` doc aligned with QuotesImpl's `validValFlags`
    def newVal(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol

    /**
      * Generates a pattern bind symbol with the given parent, name and type.
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the BindDef constructor.
      *
      *  @param parent The owner of the binding
      *  @param name The name of the binding
      *  @param flags extra flags to with which the symbol should be constructed. `Case` flag will be added. Can be `Case`
      *  @param tpe The type of the binding
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
    // Keep: `flags` doc aligned with QuotesImpl's `validBindFlags`
    def newBind(parent: Symbol, name: String, flags: Flags, tpe: TypeRepr): Symbol

    /**
      * Generate a new type symbol for a type alias with the given parent, name and type
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the TypeDef constructor.
      *
      *  @param parent The owner of the type
      *  @param name The name of the type
      *  @param flags extra flags to with which symbol can be constructed. Can be `Private` | `Protected` | `Override` | `Final` | `Infix` | `Local`
      *  @param tpe The rhs the type alias
      *  @param privateWithin the symbol within which this new type symbol should be private. May be noSymbol.
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
    @experimental
    // Keep: `flags` doc aligned with QuotesImpl's `validTypeAliasFlags`
    def newTypeAlias(parent: Symbol, name: String, flags: Flags, tpe: TypeRepr, privateWithin: Symbol): Symbol

    /**
      * Generate a new type symbol for a type bounds with the given parent, name and type
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the TypeDef constructor.
      *
      *  @param parent The owner of the type
      *  @param name The name of the type
      *  @param flags extra flags to with which symbol can be constructed. `Deferred` flag will be added. Can be `Private` | `Protected` | `Override` | `Deferred` | `Final` | `Infix` | `Local`
      *  @param tpe The bounds of the type
      *  @param privateWithin the symbol within which this new type symbol should be private. May be noSymbol.
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
    @experimental
    // Keep: `flags` doc aligned with QuotesImpl's `validBoundedTypeFlags`
    def newBoundedType(parent: Symbol, name: String, flags: Flags, tpe: TypeBounds, privateWithin: Symbol): Symbol

    /** Definition not available */
    def noSymbol: Symbol

    /**
      * A fresh name for class or member symbol names.
      *
      *  Fresh names are constructed using the following format `prefix + "$macro$" + freshIndex`.
      *  The `freshIndex` are unique within the current source file.
      *
      *  Examples: See `scala.annotation.MacroAnnotation`
      *
      *  @param prefix Prefix of the fresh name
      */
    @experimental
    def freshName(prefix: String): String

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Tree
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait TreeAPI {
    // TODO (KR) : functions
  }

  trait PackageClauseAPI {

    /** Create a package clause `package pid { stats }` */
    def apply(pid: Ref, stats: List[Tree]): PackageClause

    /** Copy a package clause `package pid { stats }` */
    def copy(original: Tree)(pid: Ref, stats: List[Tree]): PackageClause

  }

  trait StatementAPI {}

  trait ImportOrExportAPI {}

  trait ImportAPI {

    /** Create an `Import` with the given qualifier and selectors */
    def apply(expr: Term, selectors: List[Selector]): Import

    /** Copy an `Import` with the given qualifier and selectors */
    def copy(original: Tree)(expr: Term, selectors: List[Selector]): Import

  }

  trait ExportAPI {}

  trait DefinitionAPI {}

  trait ClassDefAPI {

    /**
      * Create a class definition tree
      *
      *  @param cls The class symbol. A new class symbol can be created using `Symbol.newClass`.
      *  @param parents The parents trees class. The trees must align with the parent types of `cls`.
      *                 Parents can be `TypeTree`s if they don't have term parameter,
      *                 otherwise the can be `Term` containing the `New` applied to the parameters of the extended class.
      *  @param body List of members of the class. The members must align with the members of `cls`.
      */
    // TODO add selfOpt: Option[ValDef]?
    @experimental def apply(cls: Symbol, parents: List[Term | TypeTree], body: List[Statement]): ClassDef

    def copy(original: Tree)(name: String, constr: DefDef, parents: List[Term | TypeTree], selfOpt: Option[ValDef], body: List[Statement]): ClassDef

    /**
      * Create the ValDef and ClassDef of a module (equivalent to an `object` declaration in source code).
      *
      *  Equivalent to
      *  ```
      *  def module(module: Symbol, parents: List[Tree], body: List[Statement]): (ValDef, ClassDef) =
      *    val modCls = module.moduleClass
      *    val modClassDef = ClassDef(modCls, parents, body)
      *    val modValDef = ValDef(module, Some(Apply(Select(New(TypeIdent(modCls)), cls.primaryConstructor), Nil)))
      *    List(modValDef, modClassDef)
      *  ```
      *
      *  @param module the module symbol (created using `Symbol.newModule`)
      *  @param parents parents of the module class
      *  @param body body of the module class
      *  @return The module lazy val definition and module class definition.
      *          These should be added one after the other (in that order) in the body of a class or statements of a block.
      *
      *  @syntax markdown
      */
    // TODO add selfOpt: Option[ValDef]?
    @experimental def module(module: Symbol, parents: List[Term | TypeTree], body: List[Statement]): (ValDef, ClassDef)

  }

  trait TypeDefAPI {

    def apply(symbol: Symbol): TypeDef

    def copy(original: Tree)(name: String, rhs: Tree): TypeDef

  }

  trait ValOrDefDefAPI {}

  trait DefDefAPI {

    /**
      * Create a method definition `def f[..](...)` with the signature defined in the symbol.
      *
      *  The `rhsFn` is a function that receives references to its parameters, and should return
      *  `Some` containing the implementation of the method, or `None` if the method has no implementation.
      *  Any definition directly inside the implementation should have `symbol` as owner.
      *
      *  Use `Symbol.asQuotes` to create the rhs using quoted code.
      *
      *  See also: `Tree.changeOwner`
      */
    def apply(symbol: Symbol, rhsFn: List[List[Tree]] => Option[Term]): DefDef

    def copy(original: Tree)(name: String, paramss: List[ParamClause], tpt: TypeTree, rhs: Option[Term]): DefDef

  }

  trait ValDefAPI {

    /**
      * Create a value definition `val x`, `var x` or `lazy val x` with the signature defined in the symbol.
      *
      *  The `rhs` should return `Some` containing the implementation of the method,
      *  or `None` if the method has no implementation.
      *  Any definition directly inside the implementation should have `symbol` as owner.
      *
      *  Use `Symbol.asQuotes` to create the rhs using quoted code.
      *
      *  See also: `Tree.changeOwner`
      */
    def apply(symbol: Symbol, rhs: Option[Term]): ValDef

    def copy(original: Tree)(name: String, tpt: TypeTree, rhs: Option[Term]): ValDef

    /**
      * Creates a block `{ val <name> = <rhs: Term>; <body(x): Term> }`
      *
      *  Usage:
      *  ```
      *  ValDef.let(owner, "x", rhs1) { x =>
      *    ValDef.let(x.symbol.owner, "y", rhs2) { y =>
      *      // use `x` and `y`
      *    }
      *  }
      *  ```
      */
    def let(owner: Symbol, name: String, rhs: Term)(body: Ref => Term): Term

    /**
      * Creates a block `{ val x = <rhs: Term>; <body(x): Term> }`
      *
      *  Usage:
      *  ```
      *  ValDef.let(owner, rhs1) { x =>
      *    ValDef.let(owner, rhs2) { y =>
      *      // use `x` and `y`
      *    }
      *  }
      *  ```
      */
    def let(owner: Symbol, rhs: Term)(body: Ref => Term): Term =
      let(owner, "x", rhs)(body)

    /**
      * Creates a block `{ val x1 = <terms(0): Term>; ...; val xn = <terms(n-1): Term>; <body(List(x1, ..., xn)): Term> }`
      *
      *  Usage:
      *  ```
      *  ValDef.let(owner, rhsList) { xs =>
      *     ...
      *  }
      *  ```
      */
    def let(owner: Symbol, terms: List[Term])(body: List[Ref] => Term): Term

  }

  trait TermAPI {

    /**
      * Returns a term that is functionally equivalent to `t`,
      *  however if `t` is of the form `((y1, ..., yn) => e2)(e1, ..., en)`
      *  then it optimizes the top most call by returning `Some`
      *  with the result of beta-reducing the function application.
      *  Similarly, all outermost curried function applications will be beta-reduced, if possible.
      *  Otherwise returns `None`.
      *
      *  To retain semantics the argument `ei` is bound as `val yi = ei` and by-name arguments to `def yi = ei`.
      *  Some bindings may be elided as an early optimization.
      *
      *  Example:
      *  ```scala sc:nocompile
      *  ((a: Int, b: Int) => a + b).apply(x, y)
      *  ```
      *  will be reduced to
      *  ```scala sc:nocompile
      *  val a = x
      *  val b = y
      *  a + b
      *  ```
      *
      *  Generally:
      *  ```scala sc:nocompile
      *  ([X1, Y1, ...] => (x1, y1, ...) => ... => [Xn, Yn, ...] => (xn, yn, ...) => f[X1, Y1, ..., Xn, Yn, ...](x1, y1, ..., xn, yn, ...))).apply[Tx1, Ty1, ...](myX1, myY1, ...)....apply[Txn, Tyn, ...](myXn, myYn, ...)
      *  ```
      *  will be reduced to
      *  ```scala sc:nocompile
      *  type X1 = Tx1
      *  type Y1 = Ty1
      *  ...
      *  val x1 = myX1
      *  val y1 = myY1
      *  ...
      *  type Xn = Txn
      *  type Yn = Tyn
      *  ...
      *  val xn = myXn
      *  val yn = myYn
      *  ...
      *  f[X1, Y1, ..., Xn, Yn, ...](x1, y1, ..., xn, yn, ...)
      *  ```
      */
    def betaReduce(term: Term): Option[Term]

  }

  trait RefAPI {

    /** A tree representing the same reference as the given type */
    def term(tp: TermRef): Ref

    /**
      * Create a reference tree from a symbol
      *
      *  If `sym` refers to a class member `foo` in class `C`,
      *  returns a tree representing `C.this.foo`.
      *
      *  If `sym` refers to an object member `foo` in object C, itself in prefix
      *  `pre` (which might include `.this`, if it contains a class),
      *  returns `pre.C.foo`.
      *
      *  If `sym` refers to a local definition `foo`, returns
      *  a tree representing `foo`.
      *
      *  @note In all cases, the constructed tree should only
      *  be spliced into the places where such accesses make sense.
      *  For example, it is incorrect to have `C.this.foo` outside
      *  the class body of `C`, or have `foo` outside the lexical
      *  scope for the definition of `foo`.
      */
    def apply(sym: Symbol): Ref

  }

  trait IdentAPI {

    def apply(tmref: TermRef): Term

    def copy(original: Tree)(name: String): Ident

  }

  trait WildcardAPI {
    // TODO (KR) : functions
  }

  trait SelectAPI {

    /** Select a term member by symbol */
    def apply(qualifier: Term, symbol: Symbol): Select

    /**
      * Select a field or a non-overloaded method by name
      *
      *  @note The method will produce an assertion error if the selected
      *        method is overloaded. The method `overloaded` should be used
      *        in that case.
      */
    def unique(qualifier: Term, name: String): Select

    /** Call an overloaded method with the given type and term parameters */
    def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term]): Term

    /** Call an overloaded method with the given type and term parameters */
    def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term], returnType: TypeRepr): Term

    def copy(original: Tree)(qualifier: Term, name: String): Select

  }

  trait LiteralAPI {

    /** Create a literal constant */
    def apply(constant: Constant): Literal

    def copy(original: Tree)(constant: Constant): Literal

  }

  trait ThisAPI {

    /** Create a `C.this` for `C` pointing to `cls` */
    def apply(cls: Symbol): This

    def copy(original: Tree)(qual: Option[String]): This

  }

  trait NewAPI {

    /** Create a `new <tpt: TypeTree>` */
    def apply(tpt: TypeTree): New

    def copy(original: Tree)(tpt: TypeTree): New

  }

  trait NamedArgAPI {

    /** Create a named argument `<name: String> = <value: Term>` */
    def apply(name: String, arg: Term): NamedArg

    def copy(original: Tree)(name: String, arg: Term): NamedArg

  }

  trait ApplyAPI {

    /** Create a function application `<fun: Term>(<args: List[Term]>)` */
    def apply(fun: Term, args: List[Term]): Apply

    def copy(original: Tree)(fun: Term, args: List[Term]): Apply

  }

  trait TypeApplyAPI {

    /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
    def apply(fun: Term, args: List[TypeTree]): TypeApply

    def copy(original: Tree)(fun: Term, args: List[TypeTree]): TypeApply

  }

  trait SuperAPI {

    /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
    def apply(qual: Term, mix: Option[String]): Super

    def copy(original: Tree)(qual: Term, mix: Option[String]): Super

  }

  trait AssignAPI {
    
    
      /** Create an assignment `<lhs: Term> = <rhs: Term>` */
      def apply(lhs: Term, rhs: Term): Assign

      def copy(original: Tree)(lhs: Term, rhs: Term): Assign

  }

  trait BlockAPI {
    
      /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def apply(stats: List[Statement], expr: Term): Block

      def copy(original: Tree)(stats: List[Statement], expr: Term): Block

  }

  trait ClosureAPI {
    // TODO (KR) : functions
  }

  trait IfAPI {
    // TODO (KR) : functions
  }

  trait MatchAPI {
    // TODO (KR) : functions
  }

  trait SummonFromAPI {
    // TODO (KR) : functions
  }

  trait TryAPI {
    // TODO (KR) : functions
  }

  trait ReturnAPI {
    // TODO (KR) : functions
  }

  trait RepeatedAPI {
    // TODO (KR) : functions
  }

  trait InlinedAPI {
    // TODO (KR) : functions
  }

  trait SelectOuterAPI {
    // TODO (KR) : functions
  }

  trait WhileAPI {
    // TODO (KR) : functions
  }

  trait TypedAPI {
    // TODO (KR) : functions
  }

  trait TypedOrTestAPI {
    // TODO (KR) : functions
  }

  trait BindAPI {
    // TODO (KR) : functions
  }

  trait UnapplyAPI {
    // TODO (KR) : functions
  }

  trait AlternativesAPI {
    // TODO (KR) : functions
  }

  trait CaseDefAPI {
    // TODO (KR) : functions
  }

  trait TypeCaseDefAPI {
    // TODO (KR) : functions
  }

  trait TypeTreeAPI {
    // TODO (KR) : functions
  }

  trait InferredAPI {
    // TODO (KR) : functions
  }

  trait TypeIdentAPI {
    // TODO (KR) : functions
  }

  trait TypeSelectAPI {
    // TODO (KR) : functions
  }

  trait TypeProjectionAPI {
    // TODO (KR) : functions
  }

  trait SingletonAPI {
    // TODO (KR) : functions
  }

  trait RefinedAPI {
    // TODO (KR) : functions
  }

  trait AppliedAPI {
    // TODO (KR) : functions
  }

  trait AnnotatedAPI {
    // TODO (KR) : functions
  }

  trait MatchTypeTreeAPI {
    // TODO (KR) : functions
  }

  trait ByNameAPI {
    // TODO (KR) : functions
  }

  trait LambdaTypeTreeAPI {
    // TODO (KR) : functions
  }

  trait TypeBindAPI {
    // TODO (KR) : functions
  }

  trait TypeBlockAPI {
    // TODO (KR) : functions
  }

  trait TypeBoundsTreeAPI {
    // TODO (KR) : functions
  }

  trait WildcardTypeTreeAPI {
    // TODO (KR) : functions
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      TypeRepr
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait TypeReprAPI {
    // TODO (KR) : functions
  }

  trait NamedTypeAPI {
    // TODO (KR) : functions
  }

  trait TermRefAPI {
    // TODO (KR) : functions
  }

  trait TypeRefAPI {
    // TODO (KR) : functions
  }

  trait ConstantTypeAPI {
    // TODO (KR) : functions
  }

  trait SuperTypeAPI {
    // TODO (KR) : functions
  }

  trait RefinementAPI {
    // TODO (KR) : functions
  }

  trait AppliedTypeAPI {
    // TODO (KR) : functions
  }

  trait AnnotatedTypeAPI {
    // TODO (KR) : functions
  }

  trait AndOrTypeAPI {
    // TODO (KR) : functions
  }

  trait AndTypeAPI {
    // TODO (KR) : functions
  }

  trait OrTypeAPI {
    // TODO (KR) : functions
  }

  trait MatchTypeAPI {
    // TODO (KR) : functions
  }

  trait ByNameTypeAPI {
    // TODO (KR) : functions
  }

  trait ParamRefAPI {
    // TODO (KR) : functions
  }

  trait SelfAPI {
    // TODO (KR) : functions
  }

  trait ThisTypeAPI {
    // TODO (KR) : functions
  }

  trait RecursiveThisAPI {
    // TODO (KR) : functions
  }

  trait RecursiveTypeAPI {
    // TODO (KR) : functions
  }

  trait LambdaTypeAPI {
    // TODO (KR) : functions
  }

  trait MethodOrPolyAPI {
    // TODO (KR) : functions
  }

  trait MethodTypeAPI {
    // TODO (KR) : functions
  }

  trait PolyTypeAPI {
    // TODO (KR) : functions
  }

  trait TypeLambdaAPI {
    // TODO (KR) : functions
  }

  trait MatchCaseAPI {
    // TODO (KR) : functions
  }

  trait TypeBoundsAPI {
    // TODO (KR) : functions
  }

  trait NoPrefixAPI {
    // TODO (KR) : functions
  }

  trait FlexibleTypeAPI {
    // TODO (KR) : functions
  }

}
