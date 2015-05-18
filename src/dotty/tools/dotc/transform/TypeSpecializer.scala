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

  private final def specialisedTypeToSuffix(implicit ctx: Context) =
    Map(defn.ByteType -> "$mcB$sp",
      defn.BooleanType -> "$mcZ$sp",
      defn.ShortType -> "$mcS$sp",
      defn.IntType -> "$mcI$sp",
      defn.LongType -> "$mcJ$sp",
      defn.FloatType -> "$mcF$sp",
      defn.DoubleType -> "$mcD$sp",
      defn.CharType -> "$mcC$sp",
      defn.UnitType -> "$mcV$sp")

  private def primitiveTypes(implicit ctx: Context) =
    List(defn.ByteType,
      defn.BooleanType,
      defn.ShortType,
      defn.IntType,
      defn.LongType,
      defn.FloatType,
      defn.DoubleType,
      defn.CharType,
      defn.UnitType
    )

  private def defn(implicit ctx:Context) = ctx.definitions

  private val specializationRequests: mutable.HashMap[Symbols.Symbol, List[Type]] = mutable.HashMap.empty

  /**
   *  A map that links symbols to their specialized variants.
   *  Each symbol maps to another as map, from the list of specialization types to the specialized symbol.
   */
  private val newSymbolMap: mutable.HashMap[Symbol, mutable.HashMap[List[Type], Symbols.Symbol]] = mutable.HashMap.empty

  def allowedToSpecialize(sym: Symbol, numOfTypes: Int)(implicit ctx: Context): Boolean = {
    numOfTypes <= maxTparamsToSpecialize &&
      numOfTypes > 0 &&
      sym.name != nme.asInstanceOf_ &&
      sym.name != nme.isInstanceOf_ &&
      !(sym is Flags.JavaDefined) &&
      !sym.isConstructor &&
      !sym.name.toString.contains("Function2")
  }

  def getSpecTypes(sym: Symbol, poly: PolyType)(implicit ctx: Context): List[Type] = {
    val requested = specializationRequests.getOrElse(sym, List())
    if (requested.nonEmpty) requested.toList
    else {
      if(ctx.settings.Yspecialize.value == "all") primitiveTypes
      else Nil
    }.filter(tpe => poly.paramBounds.forall(_.contains(tpe)))
  }

  def requestedSpecialization(decl: Symbol)(implicit ctx: Context): Boolean =
    specializationRequests.contains(decl) ||
      (ctx.settings.Yspecialize.value != "" && decl.name.contains(ctx.settings.Yspecialize.value)) ||
      ctx.settings.Yspecialize.value == "all"

  def registerSpecializationRequest(method: Symbols.Symbol)(arguments: List[Type])(implicit ctx: Context) = {
    if(ctx.phaseId > this.treeTransformPhase.id)
      assert(ctx.phaseId <= this.treeTransformPhase.id)
    val prev = specializationRequests.getOrElse(method, List.empty)
    specializationRequests.put(method, (arguments ::: prev).toSet.toList)
  }

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
    def generateSpecializations(remainingTParams: List[Name], specTypes: List[Type])
                               (instantiations: List[Type], names: List[String], poly: PolyType, decl: Symbol)
                               (implicit ctx: Context): List[Symbol] = {
      if (remainingTParams.nonEmpty) {
        (for (tpe <- specTypes) yield {
          generateSpecializations(remainingTParams.tail, specTypes)(tpe :: instantiations, specialisedTypeToSuffix(ctx)(tpe) :: names, poly, decl)
        }).flatten
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

      /* The following generated symbols which kept type bounds. It served, as illustrated by the `this_specialization`
       * test, as a way of keeping type bounds when instantiating a `this` referring to a generic class. However,
       * because type bounds are not transitive, this did not work out and we introduced casts instead.
       *
       * ctx.newSymbol(decl.owner, (decl.name + names.mkString).toTermName,
       *               decl.flags | Flags.Synthetic,
       *               poly.derivedPolyType(poly.paramNames,
       *                                    (poly.paramBounds zip instantiations).map
       *                                             {case (bounds, instantiation) =>
       *                                               TypeBounds(bounds.lo, AndType(bounds.hi, instantiation))},
       *                                    poly.instantiate(indices, instantiations)
       *                                    )
       *               )
       */

      val map = newSymbolMap.getOrElse(decl, mutable.HashMap.empty)
      map.put(instantiations, newSym)
      newSymbolMap.put(decl, map)
      map.values.toList
    }

    if((sym ne defn.ScalaPredefModule.moduleClass) &&
       !(sym is Flags.Package) &&
       !sym.isAnonymousClass) {
      sym.info match {
        case classInfo: ClassInfo =>
          val newDecls = classInfo.decls
            .filterNot(_.isConstructor)
            .filter(requestedSpecialization)
            .flatMap(decl => {
              decl.info.widen match {
                case poly: PolyType if allowedToSpecialize(decl.symbol, poly.paramNames.length) =>
                  generateSpecializations(poly.paramNames, getSpecTypes(decl, poly))(List.empty, List.empty, poly, decl)
                case nil => Nil
              }
          })
            val decls = classInfo.decls.cloneScope
            newDecls.foreach(decls.enter)
            classInfo.derivedClassInfo(decls = decls)
        case poly: PolyType if !newSymbolMap.contains(sym) &&
                               requestedSpecialization(sym) &&
                               allowedToSpecialize(sym, poly.paramNames.length)=>
          generateSpecializations(poly.paramNames, getSpecTypes(sym, poly))(List.empty, List.empty, poly, sym)
        case nil =>
      }
      tp
    } else tp
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {

    tree.tpe.widen match {

      case poly: PolyType if !(tree.symbol.isConstructor
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
              val tmap: (Tree => Tree) = _ match {
                case Return(t, from) if from.symbol == tree.symbol => Return(t, ref(newSym))
                case t: TypeApply => transformTypeApply(t)
                case t: Apply => transformApply(t)
                case t => t
              }

              new TreeTypeMap(
                treeMap = tmap,
                typeMap = _
                    .substDealias(origTParams, instantiations(index))
                    .subst(origVParams, vparams.flatten.map(_.tpe))
                ,
                oldOwners = tree.symbol :: Nil,
                newOwners = newSym :: Nil
              ).transform(tree.rhs)
            }})
          }
        } else Nil
        }
        val specialized_trees = specialize(tree.symbol)
        Thicket(tree :: specialized_trees)
      case _ => tree
    }
  }

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val TypeApply(fun, _) = tree
    if (fun.tpe.isParameterless) rewireTree(tree)
    tree
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val Apply(fun, args) = tree
    fun match {
      case fun: TypeApply => {
        println(
          s"""
               |args             ->  ${args}

              |f.fun            ->  ${fun.fun.tree}
             """.stripMargin)

        val newFun = rewireTree(fun)
        if (fun ne newFun) {
        val b = (args zip newFun.tpe.firstParamTypes)
        val a = b.map{
          case (arg, tpe) =>
            arg.ensureConforms(tpe)
        }
        Apply(newFun,a)
        /* zip (instantiations zip paramTypes)).map{
              case (argType, (specType, castType)) => argType.ensureConforms(specType)})*/
        } else tree
      }
      case _ => tree
    }
  }

  def rewireTree(tree: Tree)(implicit ctx: Context): Tree = {
    assert(tree.isInstanceOf[TypeApply])
    val TypeApply(fun,args) = tree
    if (newSymbolMap.contains(fun.symbol)){
      val newSymInfos = newSymbolMap(fun.symbol)
      val betterDefs = newSymInfos.filter(x => (x._1 zip args).forall{a =>
        val specializedType = a._1
        val argType = a._2
        argType.tpe <:< specializedType
      }).toList

      if (betterDefs.length > 1) {
        ctx.debuglog("Several specialized variants fit.")
        tree
      }

      else if (betterDefs.nonEmpty) {
        val best = betterDefs.head
        println(s"method ${fun.symbol.name} of ${fun.symbol.owner} rewired to specialized variant with type (${best._1})")
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
          prefix.select(best._2)
        else ref(best._2)
      } else tree
    } else tree
  }
}
