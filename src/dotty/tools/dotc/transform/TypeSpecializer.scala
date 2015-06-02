package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.TreeTypeMap
import dotty.tools.dotc.ast.Trees.SeqLiteral
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.{Symbols, Flags}
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, MiniPhaseTransform}
import dotty.tools.dotc.core.Decorators._
import scala.collection.mutable

class TypeSpecializer extends MiniPhaseTransform {
  
  override def phaseName = "specialize"

  final val maxTparamsToSpecialize = 2

  private val specializationRequests: mutable.HashMap[Symbols.Symbol, List[List[Type]]] = mutable.HashMap.empty

  def registerSpecializationRequest(method: Symbols.Symbol)(arguments: List[Type])(implicit ctx: Context) = {
    if(ctx.phaseId > this.treeTransformPhase.id)
      assert(ctx.phaseId <= this.treeTransformPhase.id)
    val prev = specializationRequests.getOrElse(method, List.empty)
    specializationRequests.put(method, arguments :: prev)
  }

  private final def name2SpecialisedType(implicit ctx: Context) =
    Map("Byte" -> ctx.definitions.ByteType,
      "Boolean" -> ctx.definitions.BooleanType,
      "Short" -> ctx.definitions.ShortType,
      "Int" -> ctx.definitions.IntType,
      "Long" -> ctx.definitions.LongType,
      "Float" -> ctx.definitions.FloatType,
      "Double" -> ctx.definitions.DoubleType,
      "Char" -> ctx.definitions.CharType,
      "Unit" -> ctx.definitions.UnitType)

  private final def specialisedType2Suffix(implicit ctx: Context) =
    Map(ctx.definitions.ByteType -> "$mcB$sp",
    ctx.definitions.BooleanType -> "$mcZ$sp",
    ctx.definitions.ShortType -> "$mcS$sp",
    ctx.definitions.IntType -> "$mcI$sp",
    ctx.definitions.LongType -> "$mcJ$sp",
    ctx.definitions.FloatType -> "$mcF$sp",
    ctx.definitions.DoubleType -> "$mcD$sp",
    ctx.definitions.CharType -> "$mcC$sp",
    ctx.definitions.UnitType -> "$mcV$sp")

  def specializeForAll(sym: Symbols.Symbol)(implicit ctx: Context): List[List[Type]] = {
    registerSpecializationRequest(sym)(specialisedType2Suffix.keys.toList)
    println("Specializing for all primitive types")
    specializationRequests.getOrElse(sym, Nil)
  }

  def specializeForSome(sym: Symbols.Symbol)(annotationArgs: List[Type])(implicit ctx: Context): List[List[Type]] = {
    registerSpecializationRequest(sym)(annotationArgs)
    println(s"specializationRequests : $specializationRequests")
    specializationRequests.getOrElse(sym, Nil)
  }

  def shouldSpecializeFor(sym: Symbols.Symbol)(implicit ctx: Context): List[List[Type]] = {
      sym.denot.getAnnotation(ctx.definitions.specializedAnnot).getOrElse(Nil) match {
        case annot: Annotation =>
          annot.arguments match {
            case List(SeqLiteral(types)) =>
              specializeForSome(sym)(types.map(tpeTree =>
                name2SpecialisedType(ctx)(tpeTree.tpe.asInstanceOf[TermRef].name.toString())))
            case List() => specializeForAll(sym)
          }
        case nil =>
          if(ctx.settings.Yspecialize.value == "all") specializeForAll(sym)
          else Nil
      }
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {

    tree.tpe.widen match {

      case poly: PolyType if !(tree.symbol.isPrimaryConstructor
                               || (tree.symbol is Flags.Label)) => {
        val origTParams = tree.tparams.map(_.symbol)
        val origVParams = tree.vparamss.flatten.map(_.symbol)
        println(s"specializing ${tree.symbol} for Tparams: $origTParams")

        def specialize(instatiations: List[Type], names: List[String]): Tree = {
          val newSym = ctx.newSymbol(tree.symbol.owner, (tree.name + names.mkString).toTermName, tree.symbol.flags | Flags.Synthetic, poly.instantiate(instatiations.toList))
          polyDefDef(newSym, { tparams => vparams => {
            assert(tparams.isEmpty)
            new TreeTypeMap(
              typeMap = _
                .substDealias(origTParams, instatiations.toList)
                .subst(origVParams, vparams.flatten.map(_.tpe)),
              oldOwners = tree.symbol :: Nil,
              newOwners = newSym :: Nil
            ).transform(tree.rhs)
          }
          })
        }

        def generateSpecializations(remainingTParams: List[TypeDef], remainingBounds: List[TypeBounds])
                                  (instatiations: List[Type],
                                    names: List[String]): Iterable[Tree] = {
          if (remainingTParams.nonEmpty) {
            val typeToSpecialize = remainingTParams.head
            val bounds = remainingBounds.head
            shouldSpecializeFor(typeToSpecialize.symbol)
              .flatten
              .filter{ tpe =>
              bounds.contains(tpe)
            }.flatMap { tpe =>
              val nme = specialisedType2Suffix(ctx)(tpe)
              generateSpecializations(remainingTParams.tail, remainingBounds.tail)(tpe :: instatiations, nme :: names)
            }
          } else
            List(specialize(instatiations.reverse, names.reverse))
        }
        Thicket(tree :: generateSpecializations(tree.tparams, poly.paramBounds)(List.empty, List.empty).toList)
      }
      case _ => tree
    }
  }
}