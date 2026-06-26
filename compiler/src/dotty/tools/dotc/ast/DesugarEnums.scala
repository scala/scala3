package dotty.tools
package dotc
package ast

import core.*
import util.Spans.*, Types.*, Contexts.*, Constants.*, Names.*, Flags.*
import Symbols.*, StdNames.*, Trees.*
import Decorators.*
import util.{Property, SourceFile}
import typer.ErrorReporting.*
import transform.SyntheticMembers.ExtendsSingletonMirror

import scala.annotation.internal.sharable

/** Helper methods to desugar enums */
object DesugarEnums {
  import untpd.*

  enum CaseKind:
    case Simple, Object, Class

  final case class EnumConstraints(minKind: CaseKind, maxKind: CaseKind, enumCases: Vector[(Int, RefTree)]):
    require(minKind.ordinal <= maxKind.ordinal && !(cached && enumCases.isEmpty))
    def requiresCreator = minKind == CaseKind.Simple
    def isEnumeration   = maxKind.ordinal < CaseKind.Class.ordinal
    def cached          = minKind.ordinal < CaseKind.Class.ordinal
  end EnumConstraints

  /** Attachment containing the number of enum cases, the smallest kind that was seen so far,
   *  and a list of all the value cases with their ordinals.
   */
  val EnumCaseCount: Property.Key[(Int, CaseKind, CaseKind, Vector[(Int, TermName)])] = Property.Key()

  /** Attachment signalling that when this definition is desugared, it should add any additional
   *  lookup methods for enums.
   */
  val DefinesEnumLookupMethods: Property.Key[Unit] = Property.Key()

  /** The enumeration class that belongs to an enum case. This works no matter
   *  whether the case is still in the enum class or it has been transferred to the
   *  companion object.
   */
  def enumClass(using Context): Symbol = {
    val cls = ctx.owner
    if (cls.is(Module)) cls.linkedClass else cls
  }

  def enumCompanion(using Context): Symbol = {
    val cls = ctx.owner
    if (cls.is(Module)) cls.sourceModule else cls.linkedClass.sourceModule
  }

  /** Is `tree` an (untyped) enum case? */
  def isEnumCase(tree: Tree)(using Context): Boolean = tree match {
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
  def interpolatedEnumParent(span: Span)(using Context): Tree = {
    val tparams = enumClass.typeParams
    def isGround(tp: Type) = tp.subst(tparams, tparams.map(_ => NoType)) eq tp
    val targs = tparams map { tparam =>
      if (tparam.is(Covariant) && isGround(tparam.info.bounds.lo))
        tparam.info.bounds.lo
      else if (tparam.is(Contravariant) && isGround(tparam.info.bounds.hi))
        tparam.info.bounds.hi
      else {
        def problem =
          if (!tparam.isOneOf(VarianceFlags)) "is invariant"
          else "has bounds that depend on a type parameter in the same parameter list"
        errorType(em"""cannot determine type argument for enum parent $enumClass,
                      |type parameter $tparam $problem""", ctx.source.atSpan(span))
      }
    }
    TypeTree(enumClass.typeRef.appliedTo(targs)).withSpan(span)
  }

  /** A type tree referring to `enumClass` */
  def enumClassRef(using Context): Tree =
    if (enumClass.exists) TypeTree(enumClass.typeRef) else TypeTree()

  /** Add implied flags to an enum class or an enum case */
  def addEnumFlags(cdef: TypeDef)(using Context): TypeDef =
    if (cdef.mods.isEnumClass) cdef.withMods(cdef.mods.withAddedFlags(Abstract | Sealed, cdef.span))
    else if (isEnumCase(cdef)) cdef.withMods(cdef.mods.withAddedFlags(Final, cdef.span))
    else cdef

  private def valuesDot(name: PreName)(implicit src: SourceFile) =
    Select(Ident(nme.DOLLAR_VALUES), name.toTermName)

  private def ArrayLiteral(values: Vector[Tree], tpt: Tree)(using Context): Tree =
    val clazzOf = TypeApply(ref(defn.Predef_classOf.termRef), tpt +: Vector())
    val ctag    = Apply(TypeApply(ref(defn.ClassTagModule_apply.termRef), tpt +: Vector()), clazzOf +: Vector())
    val apply   = Select(ref(defn.ArrayModule.termRef), nme.apply)
    Apply(Apply(TypeApply(apply, tpt +: Vector()), values), ctag +: Vector()).setApplyKind(ApplyKind.Using)

  /**  The following lists of definitions for an enum type E and known value cases e_0, ..., e_n:
   *
   *   private val $values = Array[E](this.e_0,...,this.e_n)(ClassTag[E](classOf[E]))
   *   def values = $values.clone
   *   def valueOf($name: String) = $name match {
   *     case "e_0" => this.e_0
   *     ...
   *     case "e_n" => this.e_n
   *     case _ => throw new IllegalArgumentException("case not found: " + $name)
   *   }
   */
  private def enumScaffolding(enumValues: Vector[RefTree])(using Context): Vector[Tree] = {
    val rawEnumClassRef = rawRef(enumClass.typeRef)
    extension (tpe: NamedType) def ofRawEnum = AppliedTypeTree(ref(tpe), rawEnumClassRef)

    val privateValuesDef =
      ValDef(nme.DOLLAR_VALUES, TypeTree(), ArrayLiteral(enumValues, rawEnumClassRef))
        .withFlags(Private | Synthetic)

    val valuesDef =
      DefDef(nme.values, Vector(), defn.ArrayType.ofRawEnum, valuesDot(nme.clone_))
        .withFlags(Synthetic)

    val valuesOfBody: Tree =
      val defaultCase =
        val msg = Apply(Select(Literal(Constant(s"enum ${enumClass.fullName} has no case with name: ")), nme.PLUS), Ident(nme.nameDollar))
        CaseDef(Ident(nme.WILDCARD), EmptyTree,
          Throw(New(TypeTree(defn.IllegalArgumentExceptionType), Vector(msg +: Vector()))))
      val stringCases = enumValues.map(enumValue =>
        CaseDef(Literal(Constant(enumValue.name.toString)), EmptyTree, enumValue)
      ) :+ defaultCase
      Match(Ident(nme.nameDollar), stringCases)
    val valueOfDef = DefDef(nme.valueOf, Vector(param(nme.nameDollar, defn.StringType) +: Vector()),
      TypeTree(), valuesOfBody)
        .withFlags(Synthetic)

    privateValuesDef +:
    valuesDef +:
    valueOfDef +: Vector()
  }

  private def enumLookupMethods(constraints: EnumConstraints)(using Context): Vector[Tree] =
    def scaffolding: Vector[Tree] =
      if constraints.isEnumeration then enumScaffolding(constraints.enumCases.map(_._2)) else Vector()
    def valueCtor: Vector[Tree] = if constraints.requiresCreator then enumValueCreator +: Vector() else Vector()
    def fromOrdinal: Tree =
      def throwArg(ordinal: Tree) =
        val msg = Apply(Select(Literal(Constant(s"enum ${enumClass.fullName} has no case with ordinal: ")), nme.PLUS), Select(ordinal, nme.toString_))
        Throw(New(TypeTree(defn.NoSuchElementExceptionType), Vector(msg +: Vector())))
      if !constraints.cached then
        fromOrdinalMeth(throwArg)
      else
        def default(ordinal: Tree) =
          CaseDef(Ident(nme.WILDCARD), EmptyTree, throwArg(ordinal))
        if constraints.isEnumeration then
          fromOrdinalMeth(ordinal =>
            Try(Apply(valuesDot(nme.apply), ordinal), default(ordinal) +: Vector(), EmptyTree))
        else
          fromOrdinalMeth(ordinal =>
            Match(ordinal,
              constraints.enumCases.map((i, enumValue) => CaseDef(Literal(Constant(i)), EmptyTree, enumValue))
              :+ default(ordinal)))

    if !enumClass.exists then
      // in the case of a double definition of an enum that only defines class cases (see tests/neg/i4470c.scala)
      // it seems `enumClass` might be `NoSymbol`; in this case we provide no scaffolding.
      Vector()
    else
      scaffolding ++ valueCtor :+ fromOrdinal
  end enumLookupMethods

  /** A creation method for a value of enum type `E`, which is defined as follows:
   *
   *   private def $new(_$ordinal: Int, $name: String) = new E with scala.runtime.EnumValue {
   *     def ordinal = _$ordinal // if `E` does not derive from `java.lang.Enum`
   *   }
   */
  private def enumValueCreator(using Context) = {
    val creator = New(Template(
      constr = emptyConstructor,
      parents = enumClassRef +: scalaRuntimeDot(tpnme.EnumValue) +: Vector(),
      derived = Vector(),
      self = EmptyValDef,
      body = Vector()
    ).withAttachment(ExtendsSingletonMirror, ()))
    DefDef(nme.DOLLAR_NEW,
        Vector(Vector(param(nme.ordinalDollar_, defn.IntType), param(nme.nameDollar, defn.StringType))),
        TypeTree(), creator).withFlags(Private | Synthetic)
  }

  /** Is a type parameter in `enumTypeParams` referenced from an enum class case that has
   *  given type parameters `caseTypeParams`, value parameters `vparamss` and parents `parents`?
   *  Issues an error if that is the case but the reference is illegal.
   *  The reference could be illegal for two reasons:
   *   - explicit type parameters are given
   *   - it's a value case, i.e. no value parameters are given
   */
  def typeParamIsReferenced(
    enumTypeParams: Vector[TypeSymbol],
    caseTypeParams: Vector[TypeDef],
    vparamss: Vector[Vector[ValDef]],
    parents: Vector[Tree])(using Context): Boolean = {

    object searchRef extends UntypedTreeAccumulator[Boolean] {
      var tparamNames = enumTypeParams.map(_.name).toSet[Name]
      def underBinders(binders: Vector[MemberDef], op: => Boolean): Boolean = {
        val saved = tparamNames
        tparamNames = tparamNames -- binders.map(_.name)
        try op
        finally tparamNames = saved
      }
      def apply(x: Boolean, tree: Tree)(using Context): Boolean = x || {
        tree match {
          case Ident(name) =>
            val matches = tparamNames.contains(name)
            if (matches && (caseTypeParams.nonEmpty || vparamss.isEmpty))
              report.error(em"illegal reference to type parameter $name from enum case", tree.srcPos)
            matches
          case LambdaTypeTree(lambdaParams, body) =>
            underBinders(lambdaParams, foldOver(x, tree))
          case RefinedTypeTree(parent, refinements) =>
            val refinementDefs = refinements collect { case r: MemberDef => r }
            underBinders(refinementDefs, foldOver(x, tree))
          case _ => foldOver(x, tree)
        }
      }
      def apply(tree: Tree)(using Context): Boolean =
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

    vparamss.nestedExists(valDefHasRef) || parents.exists(parentHasRef)
  }

  /** A pair consisting of
   *   - the next enum tag
   *   - scaffolding containing the necessary definitions for singleton enum cases
   *     unless that scaffolding was already generated by a previous call to `nextEnumKind`.
   */
  def nextOrdinal(name: Name, kind: CaseKind, definesLookups: Boolean)(using Context): (Int, Vector[Tree]) = {
    val (ordinal, seenMinKind, seenMaxKind, seenCases) =
      ctx.tree.removeAttachment(EnumCaseCount).getOrElse((0, CaseKind.Class, CaseKind.Simple, Vector()))
    val minKind = if kind.ordinal < seenMinKind.ordinal then kind else seenMinKind
    val maxKind = if kind.ordinal > seenMaxKind.ordinal then kind else seenMaxKind
    val cases = name match
      case name: TermName => (ordinal, name) +: seenCases
      case _              => seenCases
    if definesLookups then
      val thisRef = This(EmptyTypeIdent)
      val cachedValues = cases.reverse.map((i, name) => (i, Select(thisRef, name)))
      (ordinal, enumLookupMethods(EnumConstraints(minKind, maxKind, cachedValues)))
    else
      ctx.tree.pushAttachment(EnumCaseCount, (ordinal + 1, minKind, maxKind, cases))
      (ordinal, Vector())
  }

  def param(name: TermName, typ: Type)(using Context): ValDef = param(name, TypeTree(typ))
  def param(name: TermName, tpt: Tree)(using Context): ValDef = ValDef(name, tpt, EmptyTree).withFlags(Param)

  def ordinalMeth(body: Tree)(using Context): DefDef =
    DefDef(nme.ordinal, Vector(), TypeTree(defn.IntType), body).withAddedFlags(Synthetic)

  def ordinalMethLit(ord: Int)(using Context): DefDef =
    ordinalMeth(Literal(Constant(ord)))

  def fromOrdinalMeth(body: Tree => Tree)(using Context): DefDef =
    DefDef(nme.fromOrdinal, (param(nme.ordinal, defn.IntType) +: Vector()) +: Vector(),
      rawRef(enumClass.typeRef), body(Ident(nme.ordinal))).withFlags(Synthetic)

  /** Expand a module definition representing a parameterless enum case */
  def expandEnumModule(name: TermName, impl: Template, mods: Modifiers, definesLookups: Boolean, span: Span)(using Context): Tree = {
    assert(impl.body.isEmpty)
    if (!enumClass.exists) EmptyTree
    else if (impl.parents.isEmpty)
      expandSimpleEnumCase(name, mods, definesLookups, span)
    else {
      val (tag, scaffolding) = nextOrdinal(name, CaseKind.Object, definesLookups)
      val impl1 = cpy.Template(impl)(parents = impl.parents :+ scalaRuntimeDot(tpnme.EnumValue), body = Vector())
        .withAttachment(ExtendsSingletonMirror, ())
      val vdef = ValDef(name, TypeTree(), New(impl1)).withMods(mods.withAddedFlags(EnumValue, span))
      flatTree(vdef +: scaffolding).withSpan(span)
    }
  }

  /** Expand a simple enum case */
  def expandSimpleEnumCase(name: TermName, mods: Modifiers, definesLookups: Boolean, span: Span)(using Context): Tree =
    if (!enumClass.exists) EmptyTree
    else if (enumClass.typeParams.nonEmpty) {
      val parent = interpolatedEnumParent(span)
      val impl = Template(emptyConstructor, parent +: Vector(), Vector(), EmptyValDef, Vector())
      expandEnumModule(name, impl, mods, definesLookups, span)
    }
    else {
      val (tag, scaffolding) = nextOrdinal(name, CaseKind.Simple, definesLookups)
      val creator = Apply(Ident(nme.DOLLAR_NEW), Vector(Literal(Constant(tag)), Literal(Constant(name.toString))))
      val vdef = ValDef(name, enumClassRef, creator).withMods(mods.withAddedFlags(EnumValue, span))
      flatTree(vdef +: scaffolding).withSpan(span)
    }
}
