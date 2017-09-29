package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._
import Decorators._
import reporting.diagnostic.messages.EnumCaseDefinitionInNonEnumOwner
import collection.mutable.ListBuffer
import util.Property
import typer.ErrorReporting._

/** Helper methods to desugar enums */
object DesugarEnums {
  import untpd._
  import desugar.DerivedFromParamTree

  @sharable object CaseKind extends Enumeration {
    val Simple, Object, Class = Value
  }

  /** Attachment containing the number of enum cases and the smallest kind that was seen so far. */
  val EnumCaseCount = new Property.Key[(Int, CaseKind.Value)]

  /** the enumeration class that is a companion of the current object */
  def enumClass(implicit ctx: Context) = ctx.owner.linkedClass

  /** Is this an enum case that's situated in a companion object of an enum class? */
  def isLegalEnumCase(tree: MemberDef)(implicit ctx: Context): Boolean =
    tree.mods.hasMod[Mod.EnumCase] && enumCaseIsLegal(tree)

  /** Is enum case `tree` situated in a companion object of an enum class? */
  def enumCaseIsLegal(tree: Tree)(implicit ctx: Context): Boolean = (
    ctx.owner.is(ModuleClass) && enumClass.derivesFrom(defn.EnumClass)
    || { ctx.error(EnumCaseDefinitionInNonEnumOwner(ctx.owner), tree.pos)
         false
       }
    )

  /** A reference to the enum class `E`, possibly followed by type arguments.
   *  Each covariant type parameter is approximated by its lower bound.
   *  Each contravariant type parameter is approximated by its upper bound.
   *  It is an error if a type parameter is non-variant, or if its approximation
   *  refers to pther type parameters.
   */
  def interpolatedEnumParent(pos: Position)(implicit ctx: Context): Tree = {
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
                     |type parameter $tparam $problem""", pos)
      }
    }
    TypeTree(enumClass.typeRef.appliedTo(targs)).withPos(pos)
  }

  /** A type tree referring to `enumClass` */
  def enumClassRef(implicit ctx: Context) = TypeTree(enumClass.typeRef)

  /** Add implied flags to an enum class or an enum case */
  def addEnumFlags(cdef: TypeDef)(implicit ctx: Context) =
    if (cdef.mods.hasMod[Mod.Enum]) cdef.withFlags(cdef.mods.flags | Abstract | Sealed)
    else if (isLegalEnumCase(cdef)) cdef.withFlags(cdef.mods.flags | Final)
    else cdef

  private def valuesDot(name: String) = Select(Ident(nme.DOLLAR_VALUES), name.toTermName)
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
    def creator = New(Template(emptyConstructor, enumClassRef :: Nil, EmptyValDef,
        List(enumTagDef, toStringDef) ++ registerCall))
    DefDef(nme.DOLLAR_NEW, Nil,
        List(List(param(nme.tag, defn.IntType), param(nme.name, defn.StringType))),
        TypeTree(), creator)
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
  def expandEnumModule(name: TermName, impl: Template, mods: Modifiers, pos: Position)(implicit ctx: Context): Tree =
    if (impl.parents.isEmpty)
      if (impl.body.isEmpty)
        expandSimpleEnumCase(name, mods, pos)
      else {
        val parent = interpolatedEnumParent(pos)
        expandEnumModule(name, cpy.Template(impl)(parents = parent :: Nil), mods, pos)
      }
    else {
      def toStringMeth =
        DefDef(nme.toString_, Nil, Nil, TypeTree(defn.StringType), Literal(Constant(name.toString)))
          .withFlags(Override)
      val (tagMeth, scaffolding) = enumTagMeth(CaseKind.Object)
      val impl1 = cpy.Template(impl)(body =
        impl.body ++ List(tagMeth, toStringMeth) ++ registerCall)
      val vdef = ValDef(name, TypeTree(), New(impl1)).withMods(mods | Final)
      flatTree(scaffolding ::: vdef :: Nil).withPos(pos)
    }

  /** Expand a simple enum case */
  def expandSimpleEnumCase(name: TermName, mods: Modifiers, pos: Position)(implicit ctx: Context): Tree =
    if (enumClass.typeParams.nonEmpty) {
      val parent = interpolatedEnumParent(pos)
      val impl = Template(emptyConstructor, parent :: Nil, EmptyValDef, Nil)
      expandEnumModule(name, impl, mods, pos)
    }
    else {
      val (tag, scaffolding) = nextEnumTag(CaseKind.Simple)
      val creator = Apply(Ident(nme.DOLLAR_NEW), List(Literal(Constant(tag)), Literal(Constant(name.toString))))
      val vdef = ValDef(name, enumClassRef, creator).withMods(mods | Final)
      flatTree(scaffolding ::: vdef :: Nil).withPos(pos)
    }
}
