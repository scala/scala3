package dotty.tools
package dotc
package ast

import core._
import util.Spans._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import Symbols._, StdNames._, Trees._
import Decorators._
import util.{Property, SourceFile}
import typer.ErrorReporting._
import transform.SyntheticMembers.ExtendsSingletonMirror

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

  private def valuesDot(name: PreName)(implicit src: SourceFile) =
    Select(Ident(nme.DOLLAR_VALUES), name.toTermName)

  private def registerCall(implicit ctx: Context): List[Tree] =
    if (enumClass.typeParams.nonEmpty) Nil
    else Apply(valuesDot("register"), This(EmptyTypeIdent) :: Nil) :: Nil

  /**  The following lists of definitions for an enum type E:
   *
   *   private val $values = new EnumValues[E]
   *   def valueOf($name: String) =
   *     try $values.fromName($name) catch
   *       {
   *         case ex$:NoSuchElementException =>
   *           throw new IllegalArgumentException("key not found: ".concat(name))
   *       }
   *   def values = $values.values.toArray
   */
  private def enumScaffolding(implicit ctx: Context): List[Tree] = {
    val valuesDef =
      DefDef(nme.values, Nil, Nil, TypeTree(), Select(valuesDot(nme.values), nme.toArray))
    val privateValuesDef =
      ValDef(nme.DOLLAR_VALUES, TypeTree(),
        New(TypeTree(defn.EnumValuesType.appliedTo(enumClass.typeRef :: Nil)), ListOfNil))
        .withFlags(Private)

    val valuesOfExnMessage = Apply(
      Select(Literal(Constant("key not found: ")), "concat".toTermName),
      Ident(nme.nameDollar) :: Nil)
    val valuesOfBody = Try(
      expr = Apply(valuesDot("fromName"), Ident(nme.nameDollar) :: Nil),
      cases = CaseDef(
        pat = Typed(Ident(nme.DEFAULT_EXCEPTION_NAME), TypeTree(defn.NoSuchElementExceptionType)),
        guard = EmptyTree,
        body = Throw(New(TypeTree(defn.IllegalArgumentExceptionType), List(valuesOfExnMessage :: Nil)))
      ) :: Nil,
      finalizer = EmptyTree
    )
    val valueOfDef = DefDef(nme.valueOf, Nil, List(param(nme.nameDollar, defn.StringType) :: Nil),
      TypeTree(), valuesOfBody)

    valuesDef ::
    privateValuesDef ::
    valueOfDef :: Nil
  }

  /** A creation method for a value of enum type `E`, which is defined as follows:
   *
   *   private def $new(_$ordinal: Int, $name: String) = new E {
   *     def $ordinal = $tag
   *     override def toString = $name
   *     $values.register(this)
   *   }
   */
  private def enumValueCreator(implicit ctx: Context) = {
    val ordinalDef = ordinalMeth(Ident(nme.ordinalDollar_))
    val toStringDef = toStringMeth(Ident(nme.nameDollar))
    val creator = New(Template(
      constr = emptyConstructor,
      parents = enumClassRef :: Nil,
      derived = Nil,
      self = EmptyValDef,
      body = List(ordinalDef, toStringDef) ++ registerCall
    ).withAttachment(ExtendsSingletonMirror, ()))
    DefDef(nme.DOLLAR_NEW, Nil,
        List(List(param(nme.ordinalDollar_, defn.IntType), param(nme.nameDollar, defn.StringType))),
        TypeTree(), creator).withFlags(Private | Synthetic)
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

  /** Is a type parameter in `enumTypeParams` referenced from an enum class case that has
   *  given type parameters `caseTypeParams`, value parameters `vparamss` and parents `parents`?
   *  Issues an error if that is the case but the reference is illegal.
   *  The reference could be illegal for two reasons:
   *   - explicit type parameters are given
   *   - it's a value case, i.e. no value parameters are given
   */
  def typeParamIsReferenced(
    enumTypeParams: List[TypeSymbol],
    caseTypeParams: List[TypeDef],
    vparamss: List[List[ValDef]],
    parents: List[Tree])(implicit ctx: Context): Boolean = {

    object searchRef extends UntypedTreeAccumulator[Boolean] {
      var tparamNames = enumTypeParams.map(_.name).toSet[Name]
      def underBinders(binders: List[MemberDef], op: => Boolean): Boolean = {
        val saved = tparamNames
        tparamNames = tparamNames -- binders.map(_.name)
        try op
        finally tparamNames = saved
      }
      def apply(x: Boolean, tree: Tree)(implicit ctx: Context): Boolean = x || {
        tree match {
          case Ident(name) =>
            val matches = tparamNames.contains(name)
            if (matches && (caseTypeParams.nonEmpty || vparamss.isEmpty))
              ctx.error(i"illegal reference to type parameter $name from enum case", tree.sourcePos)
            matches
          case LambdaTypeTree(lambdaParams, body) =>
            underBinders(lambdaParams, foldOver(x, tree))
          case RefinedTypeTree(parent, refinements) =>
            val refinementDefs = refinements collect { case r: MemberDef => r }
            underBinders(refinementDefs, foldOver(x, tree))
          case _ => foldOver(x, tree)
        }
      }
      def apply(tree: Tree)(implicit ctx: Context): Boolean =
        underBinders(caseTypeParams, apply(false, tree))
    }

    def typeHasRef(tpt: Tree) = searchRef(tpt)
    def valDefHasRef(vd: ValDef) = typeHasRef(vd.tpt)
    def parentHasRef(parent: Tree): Boolean = parent match {
      case Apply(fn, _) => parentHasRef(fn)
      case TypeApply(_, targs) => targs.exists(typeHasRef)
      case Select(nu, nme.CONSTRUCTOR) => parentHasRef(nu)
      case New(tpt) => typeHasRef(tpt)
      case parent => parent.isType && typeHasRef(parent)
    }

    vparamss.exists(_.exists(valDefHasRef)) || parents.exists(parentHasRef)
  }

  /** A pair consisting of
   *   - the next enum tag
   *   - scaffolding containing the necessary definitions for singleton enum cases
   *     unless that scaffolding was already generated by a previous call to `nextEnumKind`.
   */
  def nextOrdinal(kind: CaseKind.Value)(implicit ctx: Context): (Int, List[Tree]) = {
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

  def param(name: TermName, typ: Type)(implicit ctx: Context) =
    ValDef(name, TypeTree(typ), EmptyTree).withFlags(Param)

  def ordinalMeth(body: Tree)(implicit ctx: Context): DefDef =
    DefDef(nme.ordinalDollar, Nil, Nil, TypeTree(defn.IntType), body)

  def toStringMeth(body: Tree)(implicit ctx: Context): DefDef =
    DefDef(nme.toString_, Nil, Nil, TypeTree(defn.StringType), body).withFlags(Override)

  def ordinalMethLit(ord: Int)(implicit ctx: Context): DefDef =
    ordinalMeth(Literal(Constant(ord)))

  def toStringMethLit(name: String)(implicit ctx: Context): DefDef =
    toStringMeth(Literal(Constant(name)))

  /** Expand a module definition representing a parameterless enum case */
  def expandEnumModule(name: TermName, impl: Template, mods: Modifiers, span: Span)(implicit ctx: Context): Tree = {
    assert(impl.body.isEmpty)
    if (!enumClass.exists) EmptyTree
    else if (impl.parents.isEmpty)
      expandSimpleEnumCase(name, mods, span)
    else {
      val (tag, scaffolding) = nextOrdinal(CaseKind.Object)
      val ordinalDef = ordinalMethLit(tag)
      val toStringDef = toStringMethLit(name.toString)
      val impl1 = cpy.Template(impl)(body = List(ordinalDef, toStringDef) ++ registerCall)
        .withAttachment(ExtendsSingletonMirror, ())
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
      val (tag, scaffolding) = nextOrdinal(CaseKind.Simple)
      val creator = Apply(Ident(nme.DOLLAR_NEW), List(Literal(Constant(tag)), Literal(Constant(name.toString))))
      val vdef = ValDef(name, enumClassRef, creator).withMods(mods | Final)
      flatTree(scaffolding ::: vdef :: Nil).withSpan(span)
    }
}
