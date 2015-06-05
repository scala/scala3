package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees.{Select, Ident, SeqLiteral, Typed}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.{Flags, Definitions}
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{TermRef, Type}
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, MiniPhaseTransform}

/**
 * This phase retrieves all `@specialized` anotations before they are thrown away,
 * and stores them for the `TypeSpecializer` phase.
 */
class PreSpecializer extends MiniPhaseTransform {

  override def phaseName: String = "prespecialize"

  private final def primitiveCompanionToPrimitive(companion: Type)(implicit ctx: Context) = {
    if (companion.asInstanceOf[TermRef].name.toString == "AnyRef") { // Handles `@specialized(AnyRef)` cases
      defn.AnyRefType
    }
    else {
      val claz = companion.termSymbol.companionClass
      assert(defn.ScalaValueClasses.contains(claz))
      claz.typeRef
    }
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
        sym.name != nme.isInstanceOf_ &&
        !(sym is Flags.JavaDefined) &&
        !sym.isConstructor
    }

    if (allowedToSpecialize(sym)) {
      val annotation = sym.denot.getAnnotation(defn.specializedAnnot).getOrElse(Nil)
      annotation match {
        case annot: Annotation =>
          val args = annot.arguments
          if (args.isEmpty) primitiveTypes
          else args.head match {
            case a@Typed(SeqLiteral(types), _) =>  // Matches the expected `@specialized(...)` annotations
              types.map(t => primitiveCompanionToPrimitive(t.tpe))

            case a@Select(Ident(_), _)         => primitiveTypes  // Matches `Select(Ident(Specializable), Primitives)`
                                                                  // which is used in several instances in the compiler
            case _ => ctx.error("surprising match on specialized annotation"); Nil
          }
        case nil => Nil
      }
    } else Nil
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val tparams = tree.tparams.map(_.symbol)
    val st = tparams.map(getSpec)
    if (st.nonEmpty) {
      st.map{
        case (types: List[Type]) if types.nonEmpty =>
          ctx.specializePhase.asInstanceOf[TypeSpecializer].registerSpecializationRequest(tree.symbol)(types)
        case _ =>
      }
    }
    tree
  }
}
