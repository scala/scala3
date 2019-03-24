package dotty.tools
package dotc
package ast

import core._
import util.Spans._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import Symbols._, StdNames._, Trees._
import Decorators._
import util.{Property, SourceFile}
import typer.ErrorReporting._

import scala.annotation.internal.sharable

/** Helper methods to desugar enums */
object DesugarEnums {
  import untpd._

  @sharable object CaseKind extends Enumeration {
    val Simple, Object, Class: Value = Value
  }

  /** Attachment containing the number of enum cases and the smallest kind that was seen so far. */
  val EnumCaseCount: Property.Key[(Int, DesugarEnums.CaseKind.Value)] = new Property.Key

  /** The enumeration class that belongs to an enum case. This works no matter
   *  whether the case is still in the enum class or it has been transferred to the
   *  companion object.
   */
  def enumClass(implicit ctx: Context): Symbol = {
    val cls = ctx.owner
    if (cls.is(Module)) cls.linkedClass else cls
  }

  /** Is `tree` an (untyped) enum case? */
  def isEnumCase(tree: Tree)(implicit ctx: Context): Boolean = tree match {
    case tree: MemberDef => tree.mods.isEnumCase
    case PatDef(mods, _, _, _) => mods.isEnumCase
    case _ => false
  }

  /** A reference to the enum class `E`, possibly followed by type arguments.
   *  Each covariant type parameter is approximated by its lower bound.
   *  Each contravariant type parameter is approximated by its upper bound.
   *  It is an error if a type parameter is non-variant, or if its approximation
   *  refers to pther type parameters.
   */
  def interpolatedEnumParent(span: Span)(implicit ctx: Context): Tree = {
    val tparams = enumClass.typeParams
    def isGround(tp: Type) = tp.subst(tparams, tparams.map(_ => NoType)) eq tp
    val targs = tparams map { tparam =>
      if (tparam.variance > 0 && isGround(tparam.info.bounds.lo))
        tparam.info.bounds.lo
      else if (tparam.variance < 0 && isGround(tparam.info.bounds.hi))
        tparam.info.bounds.hi
      else {
        def problem =
          if (tparam.variance == 0) "is non variant"
          else "has bounds that depend on a type parameter in the same parameter list"
        errorType(i"""cannot determine type argument for enum parent $enumClass,
                     |type parameter $tparam $problem""", ctx.source.atSpan(span))
      }
    }
    TypeTree(enumClass.typeRef.appliedTo(targs)).withSpan(span)
  }

  /** A type tree referring to `enumClass` */
  def enumClassRef(implicit ctx: Context): Tree =
    if (enumClass.exists) TypeTree(enumClass.typeRef) else TypeTree()

  /** Add implied flags to an enum class or an enum case */
  def addEnumFlags(cdef: TypeDef)(implicit ctx: Context): TypeDef =
    if (cdef.mods.isEnumClass) cdef.withMods(cdef.mods.withFlags(cdef.mods.flags | Abstract | Sealed))
    else if (isEnumCase(cdef)) cdef.withMods(cdef.mods.withFlags(cdef.mods.flags | Final))
    else cdef

  private def valuesDot(name: String)(implicit src: SourceFile) =
    Select(Ident(nme.DOLLAR_VALUES), name.toTermName)
  private def registerCall(implicit ctx: Context): List[Tree] =
    if (enumClass.typeParams.nonEmpty) Nil
    else Apply(valuesDot("register"), This(EmptyTypeIdent) :: Nil) :: Nil

  /**  The following lists of definitions for an enum type E:
   *
   *   private val $values = new EnumValues[E]
   *   def enumValue = $values.fromInt
   *   def enumValueNamed = $values.fromName
   *   def enumValues = $values.values
   */
  private def enumScaffolding(implicit ctx: Context): List[Tree] = {
    def enumDefDef(name: String, select: String) =
      DefDef(name.toTermName, Nil, Nil, TypeTree(), valuesDot(select))
    val privateValuesDef =
      ValDef(nme.DOLLAR_VALUES, TypeTree(),
        New(TypeTree(defn.EnumValuesType.appliedTo(enumClass.typeRef :: Nil)), ListOfNil))
        .withFlags(Private)
    val valueOfDef = enumDefDef("enumValue", "fromInt")
    val withNameDef = enumDefDef("enumValueNamed", "fromName")
    val valuesDef = enumDefDef("enumValues", "values")
    List(privateValuesDef, valueOfDef, withNameDef, valuesDef)
  }

  /** A creation method for a value of enum type `E`, which is defined as follows:
   *
   *   private def $new(tag: Int, name: String) = new E {
   *     def enumTag = tag
   *     override def toString = name
   *     $values.register(this)
   *   }
   */
  private def enumValueCreator(implicit ctx: Context) = {
    def param(name: TermName, typ: Type) =
      ValDef(name, TypeTree(typ), EmptyTree).withFlags(Param)
    val enumTagDef =
      DefDef(nme.enumTag, Nil, Nil, TypeTree(), Ident(nme.tag))
    val toStringDef =
      DefDef(nme.toString_, Nil, Nil, TypeTree(), Ident(nme.name))
        .withFlags(Override)
    def creator = New(Template(emptyConstructor, enumClassRef :: Nil, Nil, EmptyValDef,
        List(enumTagDef, toStringDef) ++ registerCall))
    DefDef(nme.DOLLAR_NEW, Nil,
        List(List(param(nme.tag, defn.IntType), param(nme.name, defn.StringType))),
        TypeTree(), creator)
  }

  /** The return type of an enum case apply method and any widening methods in which
   *  the apply's right hand side will be wrapped. For parents of the form
   *
   *      extends E(args) with T1(args1) with ... TN(argsN)
   *
   *  and type parameters `tparams` the generated widen method is
   *
   *      def C$to$E[tparams](x$1: E[tparams] with T1 with ... TN) = x$1
   *
   *  @param cdef            The case definition
   *  @param parents         The declared parents of the enum case
   *  @param tparams         The type parameters of the enum case
   *  @param appliedEnumRef  The enum class applied to `tparams`.
   */
  def enumApplyResult(
      cdef: TypeDef,
      parents: List[Tree],
      tparams: List[TypeDef],
      appliedEnumRef: Tree)(implicit ctx: Context): (Tree, List[DefDef]) = {

    def extractType(t: Tree): Tree = t match {
      case Apply(t1, _) => extractType(t1)
      case TypeApply(t1, ts) => AppliedTypeTree(extractType(t1), ts)
      case Select(t1, nme.CONSTRUCTOR) => extractType(t1)
      case New(t1) => t1
      case t1 => t1
    }

    val parentTypes = parents.map(extractType)
    parentTypes.head match {
      case parent: RefTree if parent.name == enumClass.name =>
        // need a widen method to compute correct type parameters for enum base class
        val widenParamType = (appliedEnumRef /: parentTypes.tail)(makeAndType)
        val widenParam = makeSyntheticParameter(tpt = widenParamType)
        val widenDef = DefDef(
          name = s"${cdef.name}$$to$$${enumClass.name}".toTermName,
          tparams = tparams,
          vparamss = (widenParam :: Nil) :: Nil,
          tpt = TypeTree(),
          rhs = Ident(widenParam.name))
        (TypeTree(), widenDef :: Nil)
      case _ =>
        (parentTypes.reduceLeft(makeAndType), Nil)
    }
  }

  /** Is a type parameter in `tparams` referenced from an enum class case that has
   *  given value parameters `vparamss` and given parents `parents`?
   */
  def typeParamIsReferenced(tparams: List[TypeSymbol], vparamss: List[List[ValDef]], parents: List[Tree])(implicit ctx: Context): Boolean = {

    val searchRef = new untpd.TreeAccumulator[Boolean] {
      var tparamNames = tparams.map(_.name).toSet[Name]
      def underBinders(binders: List[MemberDef], op: => Boolean): Boolean = {
        val saved = tparamNames
        tparamNames = tparamNames -- binders.map(_.name)
        try op
        finally tparamNames = saved
      }
      def apply(x: Boolean, tree: Tree)(implicit ctx: Context) = x || {
        tree match {
          case Ident(name) => tparamNames.contains(name)
          case LambdaTypeTree(lambdaParams, body) =>
            underBinders(lambdaParams, foldOver(x, tree))
          case RefinedTypeTree(parent, refinements) =>
            val refinementDefs = refinements collect { case r: MemberDef => r }
            underBinders(refinementDefs, foldOver(x, tree))
          case _ => foldOver(x, tree)
        }
      }
    }

    def typeHasRef(tpt: Tree) = searchRef(false, tpt)
    def valDefHasRef(vd: ValDef) = typeHasRef(vd.tpt)
    def parentHasRef(parent: Tree): Boolean = parent match {
      case Apply(fn, _) => parentHasRef(fn)
      case TypeApply(_, targs) => targs.exists(typeHasRef)
      case Select(nu, nme.CONSTRUCTOR) => parentHasRef(nu)
      case New(tpt) => typeHasRef(tpt)
      case parent if parent.isType => typeHasRef(parent)
    }

    parents.isEmpty ||  // a parent class that refers to type parameters will be generated in this case
    vparamss.exists(_.exists(valDefHasRef)) ||
    parents.exists(parentHasRef)
  }

  /** A pair consisting of
   *   - the next enum tag
   *   - scaffolding containing the necessary definitions for singleton enum cases
   *     unless that scaffolding was already generated by a previous call to `nextEnumKind`.
   */
  def nextEnumTag(kind: CaseKind.Value)(implicit ctx: Context): (Int, List[Tree]) = {
    val (count, seenKind) = ctx.tree.removeAttachment(EnumCaseCount).getOrElse((0, CaseKind.Class))
    val minKind = if (kind < seenKind) kind else seenKind
    ctx.tree.pushAttachment(EnumCaseCount, (count + 1, minKind))
    val scaffolding =
      if (enumClass.typeParams.nonEmpty || kind >= seenKind) Nil
      else if (kind == CaseKind.Object) enumScaffolding
      else if (seenKind == CaseKind.Object) enumValueCreator :: Nil
      else enumScaffolding :+ enumValueCreator
    (count, scaffolding)
  }

  /** A pair consisting of
   *   - a method returning the next enum tag
   *   - scaffolding as defined in `nextEnumTag`
   */
  def enumTagMeth(kind: CaseKind.Value)(implicit ctx: Context): (DefDef, List[Tree]) = {
    val (tag, scaffolding) = nextEnumTag(kind)
    (DefDef(nme.enumTag, Nil, Nil, TypeTree(), Literal(Constant(tag))), scaffolding)
  }

  /** Expand a module definition representing a parameterless enum case */
  def expandEnumModule(name: TermName, impl: Template, mods: Modifiers, span: Span)(implicit ctx: Context): Tree = {
    assert(impl.body.isEmpty)
    if (!enumClass.exists) EmptyTree
    else if (impl.parents.isEmpty)
      expandSimpleEnumCase(name, mods, span)
    else {
      def toStringMeth =
        DefDef(nme.toString_, Nil, Nil, TypeTree(defn.StringType), Literal(Constant(name.toString)))
          .withFlags(Override)
      val (tagMeth, scaffolding) = enumTagMeth(CaseKind.Object)
      val impl1 = cpy.Template(impl)(body = List(tagMeth, toStringMeth) ++ registerCall)
      val vdef = ValDef(name, TypeTree(), New(impl1)).withMods(mods | Final)
      flatTree(scaffolding ::: vdef :: Nil).withSpan(span)
    }
  }

  /** Expand a simple enum case */
  def expandSimpleEnumCase(name: TermName, mods: Modifiers, span: Span)(implicit ctx: Context): Tree =
    if (!enumClass.exists) EmptyTree
    else if (enumClass.typeParams.nonEmpty) {
      val parent = interpolatedEnumParent(span)
      val impl = Template(emptyConstructor, parent :: Nil, Nil, EmptyValDef, Nil)
      expandEnumModule(name, impl, mods, span)
    }
    else {
      val (tag, scaffolding) = nextEnumTag(CaseKind.Simple)
      val creator = Apply(Ident(nme.DOLLAR_NEW), List(Literal(Constant(tag)), Literal(Constant(name.toString))))
      val vdef = ValDef(name, enumClassRef, creator).withMods(mods | Final)
      flatTree(scaffolding ::: vdef :: Nil).withSpan(span)
    }
}
