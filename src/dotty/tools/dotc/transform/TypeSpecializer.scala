package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.{tpd, TreeTypeMap}
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.StringDecorator
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.{Symbols, Flags}
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, MiniPhaseTransform}
import scala.collection.mutable
import dotty.tools.dotc.core.StdNames.nme

class TypeSpecializer extends MiniPhaseTransform  with InfoTransformer {
  import tpd._
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

  private val specializationRequests: mutable.HashMap[Symbols.Symbol, List[List[Type]]] = mutable.HashMap.empty

  /**
   *  A map that links symbols to their specialized variants.
   *  Each symbol maps to another as map, from the list of specialization types to the specialized symbol.
   */
  private val newSymbolMap: mutable.HashMap[Symbol, mutable.HashMap[List[Type], Symbols.Symbol]] = mutable.HashMap.empty

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {

    def generateSpecializations(remainingTParams: List[Name], remainingBounds: List[TypeBounds], specTypes: List[Type])
                               (instantiations: List[Type], names: List[String], poly: PolyType, decl: Symbol)
                               (implicit ctx: Context): List[Symbol] = {
      if (remainingTParams.nonEmpty) {
        val bounds = remainingBounds.head
        val specializations = (for (tpe <- specTypes) yield {
          generateSpecializations(remainingTParams.tail, remainingBounds.tail, specTypes)(tpe :: instantiations, specialisedTypeToSuffix(ctx)(tpe) :: names, poly, decl)
        }).flatten
        specializations
      }
      else {
        generateSpecializedSymbols(instantiations.reverse, names.reverse, poly, decl)
      }
    }
    def generateSpecializedSymbols(instantiations: List[Type], names: List[String], poly: PolyType, decl: Symbol)
                                  (implicit ctx: Context): List[Symbol] = {
      val newSym =
        ctx.newSymbol(decl.owner, (decl.name + names.mkString).toTermName,
                      decl.flags | Flags.Synthetic, poly.instantiate(instantiations.toList))
      val map = newSymbolMap.getOrElse(decl, mutable.HashMap.empty)
      map.put(instantiations, newSym)
      newSymbolMap.put(decl, map)
      map.values.toList
    }

    if((sym ne ctx.definitions.ScalaPredefModule.moduleClass) &&
       !(sym is Flags.Package) &&
       !sym.isAnonymousClass &&
       !(sym.name == nme.asInstanceOf_)) {
      sym.info match {
        case classInfo: ClassInfo =>
          val newDecls = classInfo.decls.filterNot(_.isConstructor/*isPrimaryConstructor*/).flatMap(decl => {
            if (shouldSpecialize(decl)) {
              decl.info.widen match {
                case poly: PolyType =>
                  if (poly.paramNames.length <= maxTparamsToSpecialize && poly.paramNames.length > 0) {
                    val specTypes = getSpecTypes(sym)
                    generateSpecializations(poly.paramNames, poly.paramBounds, specTypes)(List.empty, List.empty, poly, decl)
                  }
                  else Nil
                case nil => Nil
              }
            } else Nil
          })
          if (newDecls.nonEmpty) {
            val decls = classInfo.decls.cloneScope
            newDecls.foreach(decls.enter)
            classInfo.derivedClassInfo(decls = decls)
          }
        case nil =>
      }
      tp
    } else tp
  }

  def getSpecTypes(sym: Symbol)(implicit ctx: Context): List[Type] = {
    sym.denot.getAnnotation(ctx.definitions.specializedAnnot).getOrElse(Nil) match {
      case annot: Annotation =>
        annot.arguments match {
          case List(SeqLiteral(types)) =>
            types.map(tpeTree => nameToSpecialisedType(ctx)(tpeTree.tpe.asInstanceOf[TermRef].name.toString()))
          case List() => primitiveTypes
        }
      case nil =>
        if(ctx.settings.Yspecialize.value == "all") primitiveTypes
        else Nil
    }
  }

  def shouldSpecialize(decl: Symbol)(implicit ctx: Context): Boolean =
    specializationRequests.contains(decl) ||
      (ctx.settings.Yspecialize.value != "" && decl.name.contains(ctx.settings.Yspecialize.value)) ||
      ctx.settings.Yspecialize.value == "all"

  def registerSpecializationRequest(method: Symbols.Symbol)(arguments: List[Type])(implicit ctx: Context) = {
    if(ctx.phaseId > this.treeTransformPhase.id)
      assert(ctx.phaseId <= this.treeTransformPhase.id)
    val prev = specializationRequests.getOrElse(method, List.empty)
    specializationRequests.put(method, arguments :: prev)
  }
/*
  def specializeForAll(sym: Symbols.Symbol)(implicit ctx: Context): List[Type] = {
    registerSpecializationRequest(sym)(primitiveTypes)
    println(s"Specializing $sym for all primitive types")
    specializationRequests.getOrElse(sym, Nil).flatten
  }

  def specializeForSome(sym: Symbols.Symbol)(annotationArgs: List[Type])(implicit ctx: Context): List[Type] = {
    registerSpecializationRequest(sym)(annotationArgs)
    println(s"specializationRequests : $specializationRequests")
    specializationRequests.getOrElse(sym, Nil).flatten
  }

  def specializeFor(sym: Symbols.Symbol)(implicit ctx: Context): List[Type] = {
    sym.denot.getAnnotation(ctx.definitions.specializedAnnot).getOrElse(Nil) match {
      case annot: Annotation =>
        annot.arguments match {
          case List(SeqLiteral(types)) =>
            specializeForSome(sym)(types.map(tpeTree =>
              nameToSpecialisedType(ctx)(tpeTree.tpe.asInstanceOf[TermRef].name.toString()))) // Not sure how to match TermRefs rather than type names
          case List() => specializeForAll(sym)
        }
      case nil =>
        if(ctx.settings.Yspecialize.value == "all") specializeForAll(sym)
        else Nil
    }
  }*/

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {

    tree.tpe.widen match {

      case poly: PolyType if !(tree.symbol.isConstructor//isPrimaryConstructor
        || (tree.symbol is Flags.Label))
        || (tree.symbol.name == nme.asInstanceOf_) =>
        val origTParams = tree.tparams.map(_.symbol)
        val origVParams = tree.vparamss.flatten.map(_.symbol)

        def specialize(decl : Symbol): List[Tree] = {
          if (newSymbolMap.contains(decl)) {
            val declSpecs = newSymbolMap(decl)
            val newSyms = declSpecs.values.toList
            val instantiations = declSpecs.keys.toArray
            var index = -1
            println(s"specializing ${tree.symbol} for $origTParams")
          newSyms.map { newSym =>
            index += 1
            polyDefDef(newSym.asTerm, { tparams => vparams => {
              assert(tparams.isEmpty)
              new TreeTypeMap(
                typeMap = _
                  .substDealias(origTParams, instantiations(index))
                  .subst(origVParams, vparams.flatten.map(_.tpe)),
                oldOwners = tree.symbol :: Nil,
                newOwners = newSym :: Nil
              ).transform(tree.rhs)
            }})
          }
        } else Nil
        }
        val specializedMethods = specialize(tree.symbol)
        Thicket(tree :: specializedMethods)
      case _ => tree
    }
  }

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = {

    def allowedToSpecialize(sym: Symbol): Boolean = {
      sym.name != nme.asInstanceOf_ &&
        !(sym is Flags.JavaDefined) &&
        !sym.isConstructor//isPrimaryConstructor
    }
    val TypeApply(fun,args) = tree
    if (newSymbolMap.contains(fun.symbol) && allowedToSpecialize(fun.symbol)) {
      val newSymInfos = newSymbolMap(fun.symbol)
      val betterDefs = newSymInfos.filter(x => (x._1 zip args).forall{a =>
         val specializedType = a._1
         val argType = a._2
        argType.tpe <:< specializedType
      }).toList
      assert(betterDefs.length < 2) // TODO: How to select the best if there are several ?

      if (betterDefs.nonEmpty) {
        println(s"method $fun rewired to specialozed variant with type (${betterDefs.head._1})")
        val prefix = fun match {
          case Select(pre, name) =>
            pre
          case t @ Ident(_) if t.tpe.isInstanceOf[TermRef] =>
            val tp = t.tpe.asInstanceOf[TermRef]
            if (tp.prefix ne NoPrefix)
              ref(tp.prefix.termSymbol)
            else EmptyTree
        }
        if (prefix ne EmptyTree)
          prefix.select(betterDefs.head._2)
        else ref(betterDefs.head._2)
      } else tree
    } else tree
  }
}
