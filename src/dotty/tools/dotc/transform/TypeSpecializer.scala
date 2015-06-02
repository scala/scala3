package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.TreeTypeMap
import dotty.tools.dotc.ast.Trees.SeqLiteral
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.{Symbols, Flags}
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, MiniPhaseTransform}
import scala.collection.mutable

class TypeSpecializer extends MiniPhaseTransform  with InfoTransformer {
  
  override def phaseName = "specialize"

  final val maxTparamsToSpecialize = 2

  private final def nameToSpecialisedType(implicit ctx: Context) =
    Map("Byte" -> ctx.definitions.ByteType,
      "Boolean" -> ctx.definitions.BooleanType,
      "Short" -> ctx.definitions.ShortType,
      "Int" -> ctx.definitions.IntType,
      "Long" -> ctx.definitions.LongType,
      "Float" -> ctx.definitions.FloatType,
      "Double" -> ctx.definitions.DoubleType,
      "Char" -> ctx.definitions.CharType,
      "Unit" -> ctx.definitions.UnitType)

  private final def specialisedTypeToSuffix(implicit ctx: Context) =
    Map(ctx.definitions.ByteType -> "$mcB$sp",
      ctx.definitions.BooleanType -> "$mcZ$sp",
      ctx.definitions.ShortType -> "$mcS$sp",
      ctx.definitions.IntType -> "$mcI$sp",
      ctx.definitions.LongType -> "$mcJ$sp",
      ctx.definitions.FloatType -> "$mcF$sp",
      ctx.definitions.DoubleType -> "$mcD$sp",
      ctx.definitions.CharType -> "$mcC$sp",
      ctx.definitions.UnitType -> "$mcV$sp")

  private val specializationRequests: mutable.HashMap[Symbols.Symbol, List[List[Type]]] = mutable.HashMap.empty

  private val newSymbolMap: mutable.HashMap[TermName, (List[Symbols.TermSymbol], List[Type])] = mutable.HashMap.empty // Why does the typechecker require TermSymbol ?

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {

    tp.widen match {
      case poly: PolyType if !(sym.isPrimaryConstructor
                             || (sym is Flags.Label)) =>

        def generateSpecializations(remainingTParams: List[Type], remainingBounds: List[TypeBounds])
                                      (instantiations: List[Type], names: List[String])(implicit ctx: Context): Unit = {
          if (remainingTParams.nonEmpty) {
            val typeToSpecialize = remainingTParams.head
            val bounds = remainingBounds.head
            val a = shouldSpecializeFor(typeToSpecialize.typeSymbol)  // TODO returns Nil because no annotations are found - elucidate
              a.flatten
              .filter { tpe =>
              bounds.contains(tpe)
            }.foreach({ tpe =>
              val nme = specialisedTypeToSuffix(ctx)(tpe)
              generateSpecializations(remainingTParams.tail, remainingBounds.tail)(tpe :: instantiations, nme :: names)
            })
          }
          else {
            generateSpecializedSymbols(instantiations.reverse, names.reverse)
          }
        }

        def generateSpecializedSymbols(instantiations : List[Type], names: List[String])(implicit ctx: Context): Unit = {
          val newSym = ctx.newSymbol(sym.owner, (sym.name + names.mkString).toTermName, sym.flags | Flags.Synthetic, poly.instantiate(instantiations.toList))
          ctx.enter(newSym) // TODO check frozen flag ?
          val prev = newSymbolMap.getOrElse(sym.name.toTermName, (Nil, Nil))
          val newSyms = newSym :: prev._1
          newSymbolMap.put(sym.name.toTermName, (newSyms, instantiations)) // Could `.put(...)` bring up (mutability) issues ?
        }
        val origTParams = poly.resType.paramTypess.flatten // Is this really what is needed ?
        val bounds = poly.paramBounds
        generateSpecializations(origTParams, bounds)(List.empty, List.empty)
        tp
      case _ =>
        tp
    }
  }

  def registerSpecializationRequest(method: Symbols.Symbol)(arguments: List[Type])(implicit ctx: Context) = {
    if(ctx.phaseId > this.treeTransformPhase.id)
      assert(ctx.phaseId <= this.treeTransformPhase.id)
    val prev = specializationRequests.getOrElse(method, List.empty)
    specializationRequests.put(method, arguments :: prev)
  }

  def specializeForAll(sym: Symbols.Symbol)(implicit ctx: Context): List[List[Type]] = {
    registerSpecializationRequest(sym)(specialisedTypeToSuffix.keys.toList)
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
                nameToSpecialisedType(ctx)(tpeTree.tpe.asInstanceOf[TermRef].name.toString())))
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
                               || (tree.symbol is Flags.Label)) =>
        val origTParams = tree.tparams.map(_.symbol)
        val origVParams = tree.vparamss.flatten.map(_.symbol)
        println(s"specializing ${tree.symbol} for Tparams: $origTParams")

        def specialize(instantiations: List[Type]): List[Tree] = {
          newSymbolMap(tree.name) match {
            case newSyms: (List[Symbol], List[Type]) =>
              newSyms._1.map{newSym =>
              polyDefDef(newSym, { tparams => vparams => {
                assert(tparams.isEmpty)
                new TreeTypeMap(
                  typeMap = _
                    .substDealias(origTParams, instantiations.toList)
                    .subst(origVParams, vparams.flatten.map(_.tpe)),
                  oldOwners = tree.symbol :: Nil,
                  newOwners = newSym :: Nil
                ).transform(tree.rhs)
              }
              })}
            case nil =>
              List()
          }
        }

        val specializedMethods: List[Tree] = (for (inst <- newSymbolMap.keys) yield specialize(newSymbolMap(inst)._2)).flatten.toList
        Thicket(tree :: specializedMethods)

      case _ => tree
    }
  }

  def transformTypeOfTree(tree: Tree): Tree = {
    tree
  }

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree = transformTypeOfTree(tree)
  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = transformTypeOfTree(tree)

}