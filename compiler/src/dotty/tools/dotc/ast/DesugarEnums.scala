package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._
import Decorators._
import collection.mutable.ListBuffer
import util.Property
import typer.ErrorReporting._

object DesugarEnums {
  import untpd._
  import desugar.DerivedFromParamTree

  /** Attachment containing: The number of enum cases seen so far, and whether a
   *  simple enum case was already seen.
   */
  val EnumCaseCount = new Property.Key[(Int, Boolean)]

  def enumClass(implicit ctx: Context) = ctx.owner.linkedClass

  def nextEnumTag(isSimpleCase: Boolean)(implicit ctx: Context): (Int, Boolean) = {
    val (count, simpleSeen) = ctx.tree.removeAttachment(EnumCaseCount).getOrElse((0, false))
    ctx.tree.pushAttachment(EnumCaseCount, (count + 1, simpleSeen | isSimpleCase))
    (count, simpleSeen)
  }

  def isLegalEnumCase(tree: MemberDef)(implicit ctx: Context): Boolean =
    tree.mods.hasMod[Mod.EnumCase] && enumCaseIsLegal(tree)

  def enumCaseIsLegal(tree: Tree)(implicit ctx: Context): Boolean = (
    ctx.owner.is(ModuleClass) && enumClass.derivesFrom(defn.EnumClass)
    || { ctx.error(em"case not allowed here, since owner ${ctx.owner} is not an `enum' object", tree.pos)
         false
       }
    )

  /** Type parameters reconstituted from the constructor
   *  of the `enum' class corresponding to an enum case.
   *  The variance is the same as the corresponding type parameter of the enum class.
   */
  def reconstitutedEnumTypeParams(pos: Position)(implicit ctx: Context) = {
    val tparams = enumClass.primaryConstructor.info match {
      case info: PolyType =>
        ctx.newTypeParams(ctx.newLocalDummy(enumClass), info.paramNames, EmptyFlags, info.instantiateBounds)
      case _ =>
        Nil
    }
    (tparams, enumClass.typeParams).zipped.map { (tparam, ecTparam) =>
      val tbounds = new DerivedFromParamTree
      tbounds.pushAttachment(OriginalSymbol, tparam)
      TypeDef(tparam.name, tbounds)
        .withFlags(Param | PrivateLocal | ecTparam.flags & VarianceFlags).withPos(pos)
    }
  }

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

  def enumTagMeth(implicit ctx: Context) =
    DefDef(nme.enumTag, Nil, Nil, TypeTree(),
        Literal(Constant(nextEnumTag(isSimpleCase = false)._1)))

  def enumClassRef(implicit ctx: Context) = TypeTree(enumClass.typeRef)

  def addEnumFlags(cdef: TypeDef)(implicit ctx: Context) =
    if (cdef.mods.hasMod[Mod.Enum]) cdef.withFlags(cdef.mods.flags | Abstract | Sealed)
    else if (isLegalEnumCase(cdef)) cdef.withFlags(cdef.mods.flags | Final)
    else cdef

  /**  The following lists of definitions for an enum type E:
   *
   *   private val $values = new EnumValues[E]
   *   def valueOf = $values.fromInt
   *   def withName = $values.fromName
   *   def values = $values.values
	 *
   *   private def $new(tag: Int, name: String) = new E {
   *     def enumTag = tag
   *     override def toString = name
   *     $values.register(this)
   *   }
   */
  private def enumScaffolding(implicit ctx: Context): List[Tree] = {
    def valuesDot(name: String) = Select(Ident(nme.DOLLAR_VALUES), name.toTermName)
    def enumDefDef(name: String, select: String) =
      DefDef(name.toTermName, Nil, Nil, TypeTree(), valuesDot(select))
    def param(name: TermName, typ: Type) =
      ValDef(name, TypeTree(typ), EmptyTree).withFlags(Param)
    val privateValuesDef =
      ValDef(nme.DOLLAR_VALUES, TypeTree(),
             New(TypeTree(defn.EnumValuesType.appliedTo(enumClass.typeRef :: Nil)), ListOfNil))
        .withFlags(Private)
    val valueOfDef = enumDefDef("valueOf", "fromInt")
    val withNameDef = enumDefDef("withName", "fromName")
    val valuesDef = enumDefDef("values", "values")
    val enumTagDef =
      DefDef(nme.enumTag, Nil, Nil, TypeTree(), Ident(nme.tag))
    val toStringDef =
      DefDef(nme.toString_, Nil, Nil, TypeTree(), Ident(nme.name))
        .withFlags(Override)
    val registerStat =
      Apply(valuesDot("register"), This(EmptyTypeIdent) :: Nil)
    def creator = New(Template(emptyConstructor, enumClassRef :: Nil, EmptyValDef,
        List(enumTagDef, toStringDef, registerStat)))
    val newDef =
      DefDef(nme.DOLLAR_NEW, Nil,
          List(List(param(nme.tag, defn.IntType), param(nme.name, defn.StringType))),
          TypeTree(), creator)
    List(privateValuesDef, valueOfDef, withNameDef, valuesDef, newDef)
  }

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
      val impl1 = cpy.Template(impl)(body =
        impl.body ++ List(enumTagMeth, toStringMeth))
      ValDef(name, TypeTree(), New(impl1)).withMods(mods | Final).withPos(pos)
    }

  def expandSimpleEnumCase(name: TermName, mods: Modifiers, pos: Position)(implicit ctx: Context): Tree =
    if (reconstitutedEnumTypeParams(pos).nonEmpty) {
      val parent = interpolatedEnumParent(pos)
      val impl = Template(emptyConstructor, parent :: Nil, EmptyValDef, Nil)
      expandEnumModule(name, impl, mods, pos)
    }
    else {
      val (tag, simpleSeen) = nextEnumTag(isSimpleCase = true)
      val prefix = if (simpleSeen) Nil else enumScaffolding
      val creator = Apply(Ident(nme.DOLLAR_NEW), List(Literal(Constant(tag)), Literal(Constant(name.toString))))
      val vdef = ValDef(name, enumClassRef, creator).withMods(mods | Final).withPos(pos)
      flatTree(prefix ::: vdef :: Nil).withPos(pos.startPos)
    }
}
