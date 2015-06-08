package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.{tpd, TreeTypeMap}
import dotty.tools.dotc.ast.Trees._
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
import dotty.tools._

class TypeSpecializer extends MiniPhaseTransform  with InfoTransformer {

  import tpd._
  override def phaseName = "specialize"

  final var maxTparamsToSpecialize = 0

  private final def specialisedTypeToSuffix(implicit ctx: Context) =
    Map(defn.ByteType -> "B",
      defn.BooleanType -> "Z",
      defn.ShortType -> "S",
      defn.IntType -> "I",
      defn.LongType -> "J",
      defn.FloatType -> "F",
      defn.DoubleType -> "D",
      defn.CharType -> "C",
      defn.UnitType -> "V",
      defn.AnyRefType -> "L")

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
  private val genericToInstantiation: mutable.HashMap[Symbols.Symbol, Type] = mutable.HashMap.empty
  /**
   *  A map that links symbols to their specialized variants.
   *  Each symbol maps to another map, from the list of specialization types to the specialized symbol.
   */
  private val newSymbolMap: mutable.HashMap[Symbol, mutable.HashMap[List[Type], Symbols.Symbol]] = mutable.HashMap.empty

  def allowedToSpecialize(sym: Symbol, numOfTypes: Int)(implicit ctx: Context): Boolean = {
    (maxTparamsToSpecialize == 0 || numOfTypes <= maxTparamsToSpecialize) &&
      numOfTypes > 0 &&
      sym.name != nme.asInstanceOf_ &&
      sym.name != nme.isInstanceOf_ &&
      !(sym is Flags.JavaDefined) &&
      !sym.isConstructor
  }

  def getSpecTypes(method: Symbol, poly: PolyType)(implicit ctx: Context): List[Type] = {
    val requested = specializationRequests.getOrElse(method, List.empty)
    if (requested.nonEmpty) requested
    else {
      if (ctx.settings.Yspecialize.value == "all") primitiveTypes.filter(tpe => poly.paramBounds.forall(_.contains(tpe)))
      else Nil
    }
  }

  def requestedSpecialization(decl: Symbol)(implicit ctx: Context): Boolean =
    specializationRequests.contains(decl) ||
      (ctx.settings.Yspecialize.value != "" && decl.name.contains(ctx.settings.Yspecialize.value)) ||
      ctx.settings.Yspecialize.value == "all"

  def registerSpecializationRequest(method: Symbols.Symbol)(arguments: List[Type])(implicit ctx: Context) = {
    if (ctx.phaseId > this.treeTransformPhase.id)
      assert(ctx.phaseId <= this.treeTransformPhase.id)
    val prev = specializationRequests.getOrElse(method, List.empty)
    specializationRequests.put(method, arguments ::: prev)
  }

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
    def generateSpecializations(remainingTParams: List[Name], specTypes: List[Type])
                               (instantiations: List[Type], names: List[String], poly: PolyType, decl: Symbol)
                               (implicit ctx: Context): List[Symbol] = {
      if (remainingTParams.nonEmpty) {
        specTypes.map(tpe => {
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
        ctx.newSymbol(decl.owner, (decl.name + "$mc" + names.mkString + "$sp").toTermName,
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

    if ((sym ne defn.ScalaPredefModule.moduleClass) &&
      !(sym is Flags.JavaDefined) &&
      !(sym is Flags.Scala2x) &&
      !(sym is Flags.Package) &&
      !sym.isAnonymousClass) {
      sym.info match {
        case classInfo: ClassInfo =>
          val newDecls = classInfo.decls
            .filter(_.symbol.isCompleted) // we do not want to force symbols here.
                                          // if there's unforced symbol it means its not used in the source
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
            ctx.debuglog(s"specializing ${tree.symbol} for $origTParams")
            newSyms.map { newSym =>
              index += 1
              polyDefDef(newSym.asTerm, { tparams => vparams => {
                val tmap: (Tree => Tree) = _ match {
                  case Return(t, from) if from.symbol == tree.symbol => Return(t, ref(newSym))
                  case t: TypeApply =>
                    (origTParams zip instantiations(index)).foreach(x => genericToInstantiation.put(x._1, x._2))
                    transformTypeApply(t)
                  case t: Apply =>
                    (origTParams zip instantiations(index)).foreach(x => genericToInstantiation.put(x._1, x._2))
                    transformApply(t)
                  case t => t
                }

                val typesReplaced = new TreeTypeMap(
                  treeMap = tmap,
                  typeMap = _
                    .substDealias(origTParams, instantiations(index))
                    .subst(origVParams, vparams.flatten.map(_.tpe)),
                  oldOwners = tree.symbol :: Nil,
                  newOwners = newSym :: Nil
                ).transform(tree.rhs)

                val tp = new TreeMap() {
                  // needed to workaround https://github.com/lampepfl/dotty/issues/592
                  override def transform(tree1: Tree)(implicit ctx: Context) = super.transform(tree1) match {
                    case t @ Apply(fun, args) =>
                      assert(sameLength(args, fun.tpe.widen.firstParamTypes))
                      val newArgs = (args zip fun.tpe.widen.firstParamTypes).map{case(tr, tpe) => tr.ensureConforms(tpe)}
                      if (sameTypes(args, newArgs)) {
                        t
                      } else tpd.Apply(fun, newArgs)
                    case t: ValDef =>
                      cpy.ValDef(t)(rhs = if (t.rhs.isEmpty) EmptyTree else t.rhs.ensureConforms(t.tpt.tpe))
                    case t: DefDef =>
                      cpy.DefDef(t)(rhs = if (t.rhs.isEmpty) EmptyTree else t.rhs.ensureConforms(t.tpt.tpe))
                    case t => t
                  }}
                val expectedTypeFixed = tp.transform(typesReplaced)
                expectedTypeFixed.ensureConforms(newSym.info.widen.finalResultType)
              }})
            }
          } else Nil
        }
        val specialized_trees = specialize(tree.symbol)
        Thicket(tree :: specialized_trees)
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
        val argType = genericToInstantiation.getOrElse(a._2.tpe.typeSymbol, a._2.tpe)
        argType <:< specializedType
      }).toList

      if (betterDefs.length > 1) {
        ctx.debuglog(s"Several specialized variants fit for method ${fun.symbol.name} of ${fun.symbol.owner}. Defaulting to no specialization.")
        tree
      }

      else if (betterDefs.nonEmpty) {
        val bestDef = betterDefs.head
        ctx.debuglog(s"method ${fun.symbol.name} of ${fun.symbol.owner} rewired to specialized variant with type(s) : ${bestDef._1.map{case TypeRef(_, name) => name}.mkString(", ")}")
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
          prefix.select(bestDef._2)
        else ref(bestDef._2)
      } else tree
    } else tree
  }

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val TypeApply(fun, _) = tree
    if (fun.tpe.isParameterless) rewireTree(tree)
    tree
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val Apply(fun, args) = tree
    fun match {
      case fun: TypeApply =>
        val newFun = rewireTree(fun)
        if (fun ne newFun) {
          val as = (args zip newFun.tpe.widen.firstParamTypes).map{
            case (arg, tpe) => arg.ensureConforms(tpe)
          }
          Apply(newFun,as)
        } else tree
      case fun : Apply =>
        Apply(transformApply(fun), args)
      case _ => tree
    }
  }
}
