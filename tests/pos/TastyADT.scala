object tasty {

// ------ Names --------------------------------

  trait Name
  trait PossiblySignedName

  enum TermName extends Name with PossiblySignedName {
    case Simple(str: String)
    case Qualified(prefix: TermName, selector: String)              // s"$prefix.$name"
    case Unique(underlying: TermName, separator: String, idx: Int)  // s"$underlying$separator$idx"
    case DefaultGetter(methodName: TermName, idx: String)           // s"$methodName${"$default$"}${idx+1}"
    case Variant(underlying: TermName, covariant: Boolean)          // s"${if (covariant) "+" else "-"}$underlying"
    case SuperAccessor(underlying: TermName)                        // s"${"super$"}$underlying"
    case ProtectedAccessor(underlying: TermName)                    // s"${"protectded$"}$underlying"
    case ProtectedSetter(underlying: TermName)                      // s"${"protectded$set"}$underlying"
    case ObjectClass(underlying: TermName)                          // s"$underlying${"$"}"

    case Expanded(prefix: TermName, selector: String)               // s"$prefix${"$$"}$name"  , used only for symbols coming from Scala 2
    case ExpandedPrefix(prefix: TermName, selector: String)         // s"$prefix${"$"}$name"   , used only for symbols coming from Scala 2
  }

  case class SignedName(name: TermName, resultSig: TypeName, paramSigs: List[TypeName]) extends PossiblySignedName

  case class TypeName(name: TermName) extends Name

// ------ Positions ---------------------------

  case class Position(firstOffset: Int, lastOffset: Int)

  trait Positioned {
    def pos: Position = ???
  }

// ------ Statements ---------------------------------

// Note: Definitions are written as extractors, because they may be referred to
//       recursively from some of their arguments (since we equate symbols with definitions)

  trait TopLevelStatement extends Positioned

  trait Statement extends TopLevelStatement

  case class Package(pkg: Term, body: List[TopLevelStatement]) extends TopLevelStatement

  trait Definition extends Statement {
    def tpe: Type = Type.SymRef(this, ???)
  }

  class ValDef(
    val name: TermName,
    val tpt: Term,
    rhsExp: ValDef => Term | Empty,
    val mods: List[Modifier])
  extends Definition {
    lazy val rhs = rhsExp(this)
  }
  object ValDef {
    def apply(name: TermName, tpt: Term, rhs: Term | Empty, mods: List[Modifier] = Nil) =
      new ValDef(name, tpt, _ => rhs, mods)
    def unapply(vdef: ValDef) = Some((vdef.name, vdef.tpt, vdef.rhs, vdef.mods))
  }

  class DefDef(
    val name: TermName,
    typeParamsExp: DefDef => List[TypeDef],
    paramssExp: DefDef => List[List[ValDef]],
    returnTptExp: DefDef => Term,
    rhsExp: DefDef => Term | Empty,
    val mods: List[Modifier])
  extends Definition {
    val typeParams = typeParamsExp(this)
    val paramss = paramssExp(this)
    val returnTpt = returnTptExp(this)
    lazy val rhs = rhsExp(this)
  }
  object DefDef {
    def apply(name: TermName, typeParams: List[TypeDef], paramss: List[List[ValDef]], returnTpt: Term, rhs: Term | Empty, mods: List[Modifier] = Nil) =
      new DefDef(name, _ => typeParams, _ => paramss, _ => returnTpt, _ => rhs, mods)
    def unapply(ddef: DefDef) = Some((ddef.name, ddef.typeParams, ddef.paramss, ddef.returnTpt, ddef.rhs, ddef.mods))
  }

  class TypeDef(
    val name: TypeName,
    rhsExp: TypeDef => Term,
    val mods: List[Modifier])
  extends Definition {
    val rhs = rhsExp(this),
  }
  object TypeDef {
    def apply(name: TypeName, rhs: Term, mods: List[Modifier] = Nil) = new TypeDef(name, _ => rhs, mods)
    def unapply(tdef: TypeDef) = Some((tdef.name, tdef.rhs, tdef.mods))
  }

  class ClassDef(
    val name: TypeName,
    rhsExp: ClassDef => Template,
    val mods: List[Modifier])
  extends Definition {
    val rhs = rhsExp(this)
  }
  object ClassDef {
    def apply(name: TypeName, rhs: Template, mods: List[Modifier] = Nil) = new ClassDef(name, _ => rhs, mods)
    def unapply(tdef: ClassDef) = Some((tdef.name, tdef.rhs, tdef.mods))
  }

  case class Template(
    typeParams: List[TypeDef],
    paramss: List[List[ValDef]],
    parents: List[Term],
    self: ValDef | Empty,
    body: List[Statement])

  case class Import(expr: Term, selector: List[ImportSelector]) extends Statement

  enum ImportSelector {
    case Simple(id: Id)
    case Rename(id1: Id, id2: Id)
    case Omit(id1: Id)
  }

  case class Id(name: String) extends Positioned     // untyped ident

// ------ Terms ---------------------------------

  /** Trees denoting terms */
  enum Term extends Statement {
    def tpe: Type = ???
    case Ident(name: TermName, override val tpe: Type)
    case Select(prefix: Term, name: PossiblySignedName)
    case Literal(value: Constant)
    case This(id: Id | Empty)
    case New(tpt: Term)
    case NamedArg(name: TermName, arg: Term)
    case Apply(fn: Term, args: List[Term])
    case TypeApply(fn: Term, args: List[Term])
    case Super(thiz: Term, mixin: Id | Empty)
    case Typed(expr: Term, tpt: Term)
    case Assign(lhs: Term, rhs: Term)
    case Block(stats: List[Statement], expr: Term)
    case Inlined(call: Term, bindings: List[Definition], expr: Term)
    case Lambda(method: Term, tpt: Term | Empty)
    case If(cond: Term, thenPart: Term, elsePart: Term)
    case Match(scrutinee: Term, cases: List[CaseDef])
    case Try(body: Term, catches: List[CaseDef], finalizer: Term | Empty)
    case Return(expr: Term)
    case Repeated(args: List[Term])
    case SelectOuter(from: Term, levels: Int, target: Type) // can be generated by inlining
    case Tpt(underlying: TypeTerm | Empty)
  }

  /** Trees denoting types */
  enum TypeTerm extends Positioned {
    def tpe: Type = ???
    case Ident(name: TypeName, override val tpe: Type)
    case Select(prefix: Term, name: TypeName)
    case Singleton(ref: Term)
    case Refined(underlying: TypeTerm, refinements: List[Definition])
    case Applied(tycon: TypeTerm, args: List[TypeTerm])
    case TypeBounds(loBound: TypeTerm, hiBound: TypeTerm)
    case Annotated(tpt: TypeTerm, annotation: Term)
    case And(left: TypeTerm, right: TypeTerm)
    case Or(left: TypeTerm, right: TypeTerm)
    case ByName(tpt: TypeTerm)
  }

  /** Trees denoting patterns */
  enum Pattern extends Positioned {
    def tpe: Type = ???
    case Value(v: Term)
    case Bind(name: TermName, pat: Pattern)
    case Unapply(unapply: Term, implicits: List[Term], pats: List[Pattern])
    case Alternative(pats: List[Pattern])
    case TypeTest(tpt: Term)
    case Wildcard()
  }

  case class CaseDef(pat: Pattern, guard: Term | Empty, rhs: Term) extends Positioned

  sealed trait Type

// ------ Types ---------------------------------

  object Type {
    case class ConstantType(value: Constant) extends Type
    case class SymRef(sym: Definition, qualifier: Type | Empty = Empty) extends Type
    case class NameRef(name: Name, qualifier: Type | Empty = Empty) extends Type // Empty means: select from _root_
    case class SuperType(thistp: Type, underlying: Type) extends Type
    case class Refinement(underlying: Type, name: Name, tpe: Type) extends Type
    case class AppliedType(tycon: Type, args: Type | TypeBounds) extends Type
    case class AnnotatedType(underlying: Type, annotation: Term) extends Type
    case class AndType(left: Type, right: Type) extends Type
    case class OrType(left: Type, right: Type) extends Type
    case class ByNameType(underlying: Type) extends Type
    case class ParamRef(binder: LambdaType, idx: Int) extends Type
    case class RecThis(binder: RecursiveType) extends Type

    // The following types are all expressed by extractors because they may be referred
    // to from some of their arguments

    class RecursiveType(underlyingExp: RecursiveType => Type) extends Type {
      val underlying = underlyingExp(this)
    }
    object RecursiveType {
      def unapply(tp: RecursiveType): Option[Type] = Some(tp.underlying)
    }

    trait LambdaType extends Type {
      type ParamName
      type ParamInfo
      def paramNames: List[ParamName]
      def paramInfos: List[ParamInfo]
      def resultType: Type
    }

    class MethodType(val paramNames: List[TermName], paramTypesExp: MethodType => List[Type],
                     resultTypeExp: MethodType => Type, val mods: List[Modifier]) extends LambdaType {
      type ParamName = TermName
      type ParamInfo = Type
      val paramTypes = paramTypesExp(this)
      val resultType = resultTypeExp(this)
      def paramInfos = paramTypes
    }
    object MethodType {
      def apply(paramNames: List[TermName], paramTypes: List[Type], resultType: Type, mods: List[Modifier] = Nil) =
        new MethodType(paramNames, _ => paramTypes, _ => resultType, mods)
      def unapply(tp: MethodType) = Some((tp.paramNames, tp.paramTypes, tp.resultType, tp.mods))
    }

    class PolyType(val paramNames: List[TypeName], paramBoundsExp: PolyType => List[TypeBounds],
                   resultTypeExp: PolyType => Type) extends LambdaType {
      type ParamName = TypeName
      type ParamInfo = TypeBounds
      val paramBounds = paramBoundsExp(this)
      val resultType = resultTypeExp(this)
      def paramInfos = paramBounds
    }
    object PolyType {
      def apply(paramNames: List[TypeName], paramBounds: List[TypeBounds], resultType: Type) =
        new PolyType(paramNames, _ => paramBounds, _ => resultType)
      def unapply(tp: PolyType) = Some((tp.paramNames, tp.paramBounds, tp.resultType))
    }

    class TypeLambda(val paramNames: List[TypeName], paramBoundsExp: TypeLambda => List[TypeBounds],
                     resultTypeExp: TypeLambda => Type) extends LambdaType {
      type ParamName = TypeName
      type ParamInfo = TypeBounds
      val paramBounds = paramBoundsExp(this)
      val resultType = resultTypeExp(this)
      def paramInfos = paramBounds
    }
    object TypeLambda {
      def apply(paramNames: List[TypeName], paramBounds: List[TypeBounds], resultType: Type) =
        new TypeLambda(paramNames, _ => paramBounds, _ => resultType)
      def unapply(tp: TypeLambda) = Some((tp.paramNames, tp.paramBounds, tp.resultType))
    }

    case class TypeBounds(loBound: Type, hiBound: Type)
  }

// ------ Modifiers ---------------------------------

  enum Modifier extends Positioned {
    case Private, Protected, Abstract, Final, Sealed, Case, Implicit, Erased, Lazy, Override, Inline,
         Macro,                 // inline method containing toplevel splices
         Static,                // mapped to static Java member
         Object,                // an object or its class (used for a ValDef or a ClassDef, respectively)
         Trait,                 // a trait (used for a ClassDef)
         Local,                 // used in conjunction with Private/Protected to mean private[this], proctected[this]
         Synthetic,             // generated by Scala compiler
         Artifact,              // to be tagged Java Synthetic
         Mutable,               // when used on a ValDef: a var
         Label,                 // method generated as a label
         FieldAccessor,         // a getter or setter
         CaseAcessor,           // getter for case class parameter
         Covariant,             // type parameter marked “+”
         Contravariant,         // type parameter marked “-”
         Scala2X,               // Imported from Scala2.x
         DefaultParameterized,  // Method with default parameters
         Stable                 // Method that is assumed to be stable

    case QualifiedPrivate(boundary: Type)
    case QualifiedProtected(boundary: Type)
    case Annotation(tree: Term)
  }

// ------ Constants ---------------------------------

  enum Constant(value: Any) {
    case Unit                            extends Constant(())
    case False                           extends Constant(false)
    case True                            extends Constant(true)
    case Null                            extends Constant(null)
    case Byte(value: scala.Byte)         extends Constant(value)
    case Short(value: scala.Short)       extends Constant(value)
    case Char(value: scala.Char)         extends Constant(value)
    case Int(value: scala.Int)           extends Constant(value)
    case Long(value: scala.Long)         extends Constant(value)
    case Float(value: scala.Float)       extends Constant(value)
    case Double(value: scala.Double)     extends Constant(value)
    case String(value: java.lang.String) extends Constant(value)
    case Class(value: Type)              extends Constant(value)
    case Enum(value: Type)               extends Constant(value)
  }

  sealed class Empty()
  object Empty extends Empty
}