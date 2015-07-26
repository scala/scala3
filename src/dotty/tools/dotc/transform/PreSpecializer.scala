package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees.{Ident, SeqLiteral, Typed}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols.{ClassSymbol, NoSymbol, Symbol}
import dotty.tools.dotc.core.Types.{ClassInfo, Type}
import dotty.tools.dotc.core.{Definitions, Flags}
import dotty.tools.dotc.transform.TreeTransforms.{TreeTransform, MiniPhaseTransform, TransformerInfo}

/**
 * This phase retrieves all `@specialized` anotations,
 * and stores them for the `TypeSpecializer` phase.
 */
class PreSpecializer extends MiniPhaseTransform {

  override def phaseName: String = "prespecialize"

  private var anyRefModule: Symbol = NoSymbol
  private var specializableMapping: Map[Symbol, List[Type]] = _
  private var specializableModule: Symbol = NoSymbol


  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    specializableModule = ctx.requiredModule("scala.Specializable")
    anyRefModule = ctx.requiredModule("scala.package")
    def specializableField(nm: String) = specializableModule.info.member(nm.toTermName).symbol

    specializableMapping = Map(
      specializableField("Primitives") -> List(defn.IntType, defn.LongType, defn.FloatType, defn.ShortType,
        defn.DoubleType, defn.BooleanType, defn.UnitType, defn.CharType, defn.ByteType),
      specializableField("Everything") -> List(defn.IntType, defn.LongType, defn.FloatType, defn.ShortType,
        defn.DoubleType, defn.BooleanType, defn.UnitType, defn.CharType, defn.ByteType, defn.AnyRefType),
      specializableField("Bits32AndUp") -> List(defn.IntType, defn.LongType, defn.FloatType, defn.DoubleType),
      specializableField("Integral") -> List(defn.ByteType, defn.ShortType, defn.IntType, defn.LongType, defn.CharType),
      specializableField("AllNumeric") -> List(defn.ByteType, defn.ShortType, defn.IntType, defn.LongType,
        defn.CharType, defn.FloatType, defn.DoubleType),
      specializableField("BestOfBreed") -> List(defn.IntType, defn.DoubleType, defn.BooleanType, defn.UnitType,
        defn.AnyRefType)
    )
    this
  }

  private final def primitiveCompanionToPrimitive(companion: Type)(implicit ctx: Context) = {
    if (companion.termSymbol eq anyRefModule.info.member(nme.AnyRef.toTermName).symbol) {
      defn.AnyRefType
    }
    else {
      val claz = companion.termSymbol.companionClass
      assert(defn.ScalaValueClasses.contains(claz))
      claz.typeRef
    }
  }

  private def specializableToPrimitive(specializable: Type, name: Name)(implicit ctx: Context): List[Type] = {
    if (specializable.termSymbol eq specializableModule.info.member(name).symbol) {
      specializableMapping(specializable.termSymbol)
    }
    else Nil
  }

  def defn(implicit ctx: Context): Definitions = ctx.definitions

  private def primitiveTypes(implicit ctx: Context) =
    List(ctx.definitions.ByteType,
      ctx.definitions.BooleanType,
      ctx.definitions.ShortType,
      ctx.definitions.IntType,
      ctx.definitions.LongType,
      ctx.definitions.FloatType,
      ctx.definitions.DoubleType,
      ctx.definitions.CharType,
      ctx.definitions.UnitType
    )

  def getSpec(sym: Symbol)(implicit ctx: Context): List[Type] = {

    def allowedToSpecialize(sym: Symbol): Boolean = {
      sym.name != nme.asInstanceOf_ &&
        !(sym is Flags.JavaDefined) &&
        !sym.isPrimaryConstructor
    }

    if (allowedToSpecialize(sym)) {
      val annotation = sym.denot.getAnnotation(defn.SpecializedAnnot).getOrElse(Nil)
      annotation match {
        case annot: Annotation =>
          val args = annot.arguments
          if (args.isEmpty) primitiveTypes
          else args.head match {
            case a @ Typed(SeqLiteral(types), _) =>
              types.map(t => primitiveCompanionToPrimitive(t.tpe))
            case a @ Ident(groupName) => // Matches `@specialized` annotations on Specializable Groups
              specializableToPrimitive(a.tpe.asInstanceOf[Type], groupName)
            case _ => ctx.error("unexpected match on specialized annotation"); Nil
          }
        case nil => Nil
      }
    } else Nil
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val tparams = tree.tparams.map(_.symbol)
    val st = tparams.zipWithIndex.map{case(sym, i) => (i, getSpec(sym))}
    sendRequests(st, tree)
    tree
  }

  def sendRequests(requests: List[(Int, List[Type])], tree: tpd.Tree)(implicit ctx: Context): Unit = {
    if (requests.nonEmpty) {
      requests.map{
        case (index, types) if types.nonEmpty =>
          ctx.specializePhase.asInstanceOf[TypeSpecializer].registerSpecializationRequest(tree.symbol)(index, types)
        case _ =>
      }
    }
  }
}
