package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._
import Decorators._
import collection.mutable.ListBuffer
import util.Property
import reporting.diagnostic.messages._

object DesugarEnums {
  import untpd._
  import desugar.DerivedFromParamTree

  val EnumCaseCount = new Property.Key[Int]

  def enumClass(implicit ctx: Context) = ctx.owner.linkedClass

  def nextEnumTag(implicit ctx: Context): Int = {
    val result = ctx.tree.removeAttachment(EnumCaseCount).getOrElse(0)
    ctx.tree.pushAttachment(EnumCaseCount, result + 1)
    result
  }

  def isLegalEnumCase(tree: MemberDef)(implicit ctx: Context): Boolean = {
    tree.mods.hasMod[Mod.EnumCase] &&
    (  ctx.owner.is(ModuleClass) && enumClass.derivesFrom(defn.EnumClass)
    || { ctx.error(em"case not allowed here, since owner ${ctx.owner} is not an `enum' object", tree.pos)
         false
       }
    )
  }

  /** Type parameters reconstituted from the constructor
   *  of the `enum' class corresponding to an enum case
   */
  def reconstitutedEnumTypeParams(pos: Position)(implicit ctx: Context) = {
    val tparams = enumClass.primaryConstructor.info match {
      case info: PolyType =>
        ctx.newTypeParams(ctx.newLocalDummy(enumClass), info.paramNames, EmptyFlags, info.instantiateBounds)
      case _ =>
        Nil
    }
    for (tparam <- tparams) yield {
      val tbounds = new DerivedFromParamTree
      tbounds.pushAttachment(OriginalSymbol, tparam)
      TypeDef(tparam.name, tbounds)
        .withFlags(Param | PrivateLocal).withPos(pos)
    }
  }

  def enumTagMeth(implicit ctx: Context) =
    DefDef(nme.enumTag, Nil, Nil, TypeTree(), Literal(Constant(nextEnumTag)))

  def enumClassRef(implicit ctx: Context) = TypeTree(enumClass.typeRef)

  def addEnumFlags(cdef: TypeDef)(implicit ctx: Context) =
    if (cdef.mods.hasMod[Mod.Enum]) cdef.withFlags(cdef.mods.flags | Abstract | Sealed)
    else if (isLegalEnumCase(cdef)) cdef.withFlags(cdef.mods.flags | Final)
    else cdef

  /**  The following lists of definitions for an enum type E:
   *
   *   private val $values = new EnumValues[E]
   *   def valueOf: Int => E = $values
   *   def values = $values.values
	 *
   *   private def $new(tag: Int, name: String) = new E {
   *     def enumTag = tag
   *     override def toString = name
   *     $values.register(this)
   *   }
   */
  private def enumScaffolding(implicit ctx: Context): List[Tree] = {
    val valsRef = Ident(nme.DOLLAR_VALUES)
    def param(name: TermName, typ: Type) =
      ValDef(name, TypeTree(typ), EmptyTree).withFlags(Param)
    val privateValuesDef =
      ValDef(nme.DOLLAR_VALUES, TypeTree(),
             New(TypeTree(defn.EnumValuesType.appliedTo(enumClass.typeRef :: Nil)), ListOfNil))
        .withFlags(Private)
    val valueOfDef =
      DefDef(nme.valueOf, Nil, Nil,
          TypeTree(defn.FunctionOf(defn.IntType :: Nil, enumClass.typeRef)), valsRef)
    val valuesDef =
      DefDef(nme.values, Nil, Nil, TypeTree(), Select(valsRef, nme.values))
    val enumTagDef =
      DefDef(nme.enumTag, Nil, Nil, TypeTree(), Ident(nme.tag))
    val toStringDef =
      DefDef(nme.toString_, Nil, Nil, TypeTree(), Ident(nme.name))
        .withFlags(Override)
    val registerStat =
      Apply(Select(valsRef, nme.register), This(EmptyTypeIdent) :: Nil)
    def creator = New(Template(emptyConstructor, enumClassRef :: Nil, EmptyValDef,
        List(enumTagDef, toStringDef, registerStat)))
    val newDef =
      DefDef(nme.DOLLAR_NEW, Nil,
          List(List(param(nme.tag, defn.IntType), param(nme.name, defn.StringType))),
          TypeTree(), creator)
    List(privateValuesDef, valueOfDef, valuesDef, newDef)
  }

  def expandEnumModule(name: TermName, impl: Template, mods: Modifiers, pos: Position)(implicit ctx: Context): Tree = {
    def nameLit = Literal(Constant(name.toString))
    if (impl.parents.isEmpty) {
      if (reconstitutedEnumTypeParams(pos).nonEmpty)
        ctx.error(i"illegal enum value of generic $enumClass: an explicit `extends' clause is needed", pos)
      val tag = nextEnumTag
      val prefix = if (tag == 0) enumScaffolding else Nil
      val creator = Apply(Ident(nme.DOLLAR_NEW), List(Literal(Constant(tag)), nameLit))
      val vdef = ValDef(name, enumClassRef, creator).withMods(mods | Final).withPos(pos)
      flatTree(prefix ::: vdef :: Nil).withPos(pos.startPos)
    } else {
      def toStringMeth =
        DefDef(nme.toString_, Nil, Nil, TypeTree(defn.StringType), nameLit)
          .withFlags(Override)
      val impl1 = cpy.Template(impl)(body =
        impl.body ++ List(enumTagMeth, toStringMeth))
      ValDef(name, TypeTree(), New(impl1)).withMods(mods | Final).withPos(pos)
    }
  }
}
