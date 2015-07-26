package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.{tpd, TreeTypeMap}
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.{NameOps, Symbols, Flags}
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, MiniPhaseTransform}
import scala.collection.mutable
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools._

import scala.collection.mutable.ListBuffer

class TypeSpecializer extends MiniPhaseTransform  with InfoTransformer {

  import tpd._
  override def phaseName = "specialize"

  private def primitiveTypes(implicit ctx: Context) =
    List(defn.ByteType,
      defn.BooleanType,
      defn.ShortType,
      defn.IntType,
      defn.LongType,
      defn.FloatType,
      defn.DoubleType,
      defn.CharType,
      defn.UnitType)

  private def defn(implicit ctx:Context) = ctx.definitions

  /**
   *  Methods requested for specialization
   *  Generic Symbol   =>  List[  (position in type args list, specialized type requested)  ]
   */
  private val specializationRequests: mutable.HashMap[Symbols.Symbol, List[(Int, List[Type])]] = mutable.HashMap.empty

  /**
   *  A list of instantiation values of generics (helps with recursive polymorphic methods)
   */
  private val genericToInstantiation: mutable.HashMap[Symbols.Symbol, Type] = mutable.HashMap.empty

  /**
   *  A map that links symbols to their specialized variants.
   *  Each symbol maps to another map, from the list of specialization types to the specialized symbol.
   *  Generic symbol  =>  Map[  Tuple(position in type args list, specialized Type)  =>  Specialized Symbol ]
   */
  private val newSymbolMap: mutable.HashMap[Symbol, mutable.HashMap[List[(Int, Type)], Symbols.Symbol]] = mutable.HashMap.empty

  /**
   *  A map from specialised symbols to the indices of their remaining generic types
   */
  private val newSymbolsGenericIndices: mutable.HashMap[Symbol, List[Int]] = mutable.HashMap.empty

  /**
   *  A list of symbols gone through the specialisation pipeline
   *  Is used to make calls to transformInfo idempotent
   */
  private val specialized: ListBuffer[Symbol] = ListBuffer.empty

  def allowedToSpecialize(sym: Symbol, numOfTypes: Int)(implicit ctx: Context) =
    numOfTypes > 0 &&
      sym.name != nme.asInstanceOf_ &&
      !newSymbolMap.contains(sym) &&
      !sym.name.toString.contains("$sp") &&
      !(sym is Flags.JavaDefined) &&
      !sym.isPrimaryConstructor


  def getSpecTypes(method: Symbol, poly: PolyType)(implicit ctx: Context): List[(Int, List[Type])] = {

    val requested = specializationRequests.getOrElse(method, List.empty).toMap
    if (requested.nonEmpty) {
      poly.paramNames.zipWithIndex.map{case(name, i) => (i, requested.getOrElse(i, Nil))}
    }
    else {
      if (ctx.settings.Yspecialize.value > 0) {
        val filteredPrims = primitiveTypes.filter(tpe => poly.paramBounds.forall(_.contains(tpe)))
        List.range(0, Math.min(poly.paramNames.length, ctx.settings.Yspecialize.value)).map(i => (i, filteredPrims))
      }
      else Nil
    }
  }

  def requestedSpecialization(decl: Symbol)(implicit ctx: Context): Boolean =
    ctx.settings.Yspecialize.value != 0 || specializationRequests.contains(decl)

  def registerSpecializationRequest(method: Symbols.Symbol)(index: Int, arguments: List[Type])(implicit ctx: Context) = {
    if (ctx.phaseId > this.treeTransformPhase.id)
      assert(ctx.phaseId <= this.treeTransformPhase.id)
    val prev = specializationRequests.getOrElse(method, List.empty)
    specializationRequests.put(method, (index, arguments) :: prev)
  }

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {

    def enterNewSyms(newDecls: List[Symbol], classInfo: ClassInfo) = {
      val decls = classInfo.decls.cloneScope
      newDecls.foreach(decls.enter)
      classInfo.derivedClassInfo(decls = decls)
    }

    def specializeMethods(sym: Symbol) = {
      specialized += sym
      sym.info match {
        case classInfo: ClassInfo =>
          val newDecls = classInfo.decls
            .filter(_.symbol.isCompleted) // We do not want to force symbols. Unforced symbol are not used in the source
            .filterNot(_.isConstructor)
            .filter(requestedSpecialization)
            .flatMap(decl => {
            decl.info.widen match {
              case poly: PolyType if allowedToSpecialize(decl.symbol, poly.paramNames.length) =>
                generateMethodSpecializations(getSpecTypes(decl, poly))(List.empty, poly, decl)
              case _ => Nil
            }
          })

          if (newDecls.nonEmpty) enterNewSyms(newDecls.toList, classInfo)
          else tp
        case poly: PolyType if allowedToSpecialize(sym, poly.paramNames.length) =>
          if (sym.owner.info.isInstanceOf[ClassInfo]) {
            transformInfo(sym.owner.info, sym.owner)
            tp
          }
          else if (requestedSpecialization(sym) &&
            allowedToSpecialize(sym, poly.paramNames.length)) {
            generateMethodSpecializations(getSpecTypes(sym, poly))(List.empty, poly, sym)
            tp
          }
          else tp
        case _ => tp
      }
    }

    def generateMethodSpecializations(specTypes: List[(Int, List[Type])])
                                     (instantiations: List[(Int, Type)], poly: PolyType, decl: Symbol)
                                     (implicit ctx: Context): List[Symbol] = {
      if (specTypes.nonEmpty) {
        specTypes.head match{
          case (i, tpes) if tpes.nonEmpty =>
            tpes.flatMap(tpe =>
              generateMethodSpecializations(specTypes.tail)((i, tpe) :: instantiations, poly, decl)
            )
          case (i, nil) =>
            generateMethodSpecializations(specTypes.tail)(instantiations, poly, decl)
        }
      }
      else {
        if (instantiations.isEmpty) Nil
        else generateSpecializedSymbol(instantiations.reverse, poly, decl) :: Nil
      }
    }
    def generateSpecializedSymbol(instantiations: List[(Int, Type)], poly: PolyType, decl: Symbol)
                                 (implicit ctx: Context): Symbol = {
      val indices = instantiations.map(_._1)
      val instanceTypes = instantiations.map(_._2)
      val newSym = ctx.newSymbol(decl.owner, NameOps.NameDecorator(decl.name).specializedFor(Nil, Nil, instanceTypes, instanceTypes.map(_.asInstanceOf[NamedType].name)),
      decl.flags | Flags.Synthetic, {
        if (indices.length != poly.paramNames.length) // Partial Specialisation case
          poly.instantiate(indices, instanceTypes) // Returns a PolyType with uninstantiated types kept generic
        else
          poly.instantiate(instanceTypes) // Returns a MethodType, as no polymorphic types remains
      })

      val map = newSymbolMap.getOrElse(decl, mutable.HashMap.empty)
      map.put(instantiations, newSym)
      newSymbolMap.put(decl, map)

      newSymbolsGenericIndices.put(newSym, indices)

      newSym
    }

    if (!specialized.contains(sym) &&
      (sym ne defn.ScalaPredefModule.moduleClass) &&
      !(sym is Flags.JavaDefined) &&
      !(sym is Flags.Scala2x) &&
      !(sym is Flags.Package) &&
      !sym.isAnonymousClass) {
      specializeMethods(sym)
    } else tp
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {

    tree.tpe.widen match {

      case poly: PolyType
        if !(tree.symbol.isPrimaryConstructor
          || (tree.symbol is Flags.Label)
          ) =>
        val origTParams = tree.tparams.map(_.symbol)
        val origVParams = tree.vparamss.flatten.map(_.symbol)

        def specialize(decl : Symbol): List[Tree] = {

          def makeTypesList(origTSyms: List[Symbol], instantiation: Map[Int, Type], pt: PolyType): List[Type] = {
            var holePos = -1
            origTSyms.zipWithIndex.map {
              case (_, i) => instantiation.getOrElse(i, {
                holePos += 1
                PolyParam(pt, holePos)
              }
              ).widen
            }
          }

          if (newSymbolMap.contains(decl)) {
            val declSpecs = newSymbolMap(decl)
            val newSyms = declSpecs.values.toList
            val instantiations = declSpecs.keys.toArray
            var index = -1
            ctx.debuglog(s"specializing ${tree.symbol} for $origTParams")
            newSyms.map { newSym =>
              index += 1
              val newSymType = newSym.info.widenDealias
              polyDefDef(newSym.asTerm, { tparams => vparams => {
                val instTypes = newSymType match {
                  case pt: PolyType => makeTypesList(origTParams, instantiations(index).toMap, pt)
                  case _ => instantiations(index).map(_._2)
                }

                val treemap: (Tree => Tree) = _ match {
                  case Return(t, from) if from.symbol == tree.symbol => Return(t, ref(newSym))
                  case t: TypeApply =>
                    (origTParams zip instTypes).foreach(x => genericToInstantiation.put(x._1, x._2))
                    transformTypeApply(t)
                  case t: Apply =>
                    (origTParams zip instTypes).foreach(x => genericToInstantiation.put(x._1, x._2))
                    transformApply(t)
                  case t => t
                }

                val abstractPolyType = tree.symbol.info.widenDealias.asInstanceOf[PolyType]
                val typemap = new TypeMap {
                  override def apply(tp: Type): Type = {
                    val t = mapOver(tp)
                      .substDealias(origTParams, instTypes)
                      .substParams(abstractPolyType, instTypes)
                      .subst(origVParams, vparams.flatten.map(_.tpe))
                    newSymType match {
                      case mt: MethodType if tparams.isEmpty =>
                        t.substParams(newSymType.asInstanceOf[MethodType], vparams.flatten.map(_.tpe))
                      case pt: PolyType =>
                        t.substParams(newSymType.asInstanceOf[PolyType], tparams)
                          .substParams(newSymType.resultType.asInstanceOf[MethodType], vparams.flatten.map(_.tpe))
                      case _ => t
                    }
                  }
                }

                val typesReplaced = new TreeTypeMap(
                  treeMap = treemap,
                  typeMap = typemap,
                  oldOwners = tree.symbol :: Nil,
                  newOwners = newSym :: Nil
                ).transform(tree.rhs)

                val tp = new TreeMap() {
                  // needed to workaround https://github.com/lampepfl/dotty/issues/592
                  override def transform(tree1: Tree)(implicit ctx: Context) = super.transform(tree1) match {
                    case t @ Apply(fun, args) =>
                      assert(sameLength(args, fun.tpe.widen.firstParamTypes),
                        s"Wrong number of parameters. Expected: ${fun.tpe.widen.firstParamTypes.length}. Found: ${args.length}")
                      val newArgs = (args zip fun.tpe.widen.firstParamTypes).map{
                        case(tr, tpe) =>
                          assert(tpe.widen ne NoType, "Bad cast when specializing")
                          tr.ensureConforms(typemap(tpe.widen))
                      }
                      if (sameTypes(args, newArgs)) {
                        t
                      }
                      else tpd.Apply(fun, newArgs)
                    case t: ValDef =>
                      cpy.ValDef(t)(rhs = if (t.rhs.isEmpty) EmptyTree else
                        t.rhs.ensureConforms(t.tpt.tpe))
                    case t: DefDef =>
                      cpy.DefDef(t)(rhs = if (t.rhs.isEmpty) EmptyTree else
                        t.rhs.ensureConforms(t.tpt.tpe))
                    case t: TypeTree =>
                      t.tpe match {
                        case pp: PolyParam =>
                          TypeTree(tparams(pp.paramNum))
                        case _ => t
                      }
                    case t => t
                  }}
                val expectedTypeFixed = tp.transform(typesReplaced)
                if (expectedTypeFixed ne EmptyTree) {
                  expectedTypeFixed.ensureConforms(typemap(newSym.info.widen.finalResultType.widenDealias))
                }
                else expectedTypeFixed
              }})
            }
          } else Nil
        }
        val specializedTrees = specialize(tree.symbol)
        Thicket(tree :: specializedTrees)
      case _ => tree
    }
  }

  def rewireTree(tree: Tree)(implicit ctx: Context): Tree = {
    assert(tree.isInstanceOf[TypeApply])
    val TypeApply(fun,args) = tree
    if (newSymbolMap.contains(fun.symbol)){
      val newSymInfos = newSymbolMap(fun.symbol)
      val betterDefs = newSymInfos.filter(
        x => {
          val instantiation = x._1
          instantiation.forall { x =>
            val ord = x._1
            val tp = x._2
            args(ord).tpe <:< tp
          }}).toList

      if (betterDefs.length > 1) {
        ctx.debuglog(s"Several specialized variants fit for ${fun.symbol.name} of ${fun.symbol.owner}. Defaulting to no specialization.")
        tree
      }

      else if (betterDefs.nonEmpty) {
        val bestDef = betterDefs.head
        ctx.debuglog(s"method ${fun.symbol.name} of ${fun.symbol.owner} rewired to specialized variant")
        val prefix = fun match {
          case Select(pre, name) =>
            pre
          case t @ Ident(_) if t.tpe.isInstanceOf[TermRef] =>
            val tp = t.tpe.asInstanceOf[TermRef]
            if (tp.prefix ne NoPrefix)
              ref(tp.prefix.termSymbol)
            else EmptyTree
          case _ => EmptyTree
        }
        if (prefix ne EmptyTree) prefix.select(bestDef._2)
        else ref(bestDef._2)
      } else tree
    } else tree
  }

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val TypeApply(fun, _) = tree
    if (fun.tpe.isParameterless) rewireTree(tree)
    else tree
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val Apply(fun, args) = tree
    fun match {
      case fun: TypeApply =>
        val TypeApply(_, typeArgs) = fun
        val newFun = rewireTree(fun)
        if (fun ne newFun) {
          newFun.symbol.info.widenDealias match {
            case pt: PolyType =>  // Need to apply types to the remaining generics first
              val tpeOfRemainingGenerics = typeArgs.zipWithIndex.filterNot(x => newSymbolsGenericIndices(newFun.symbol).contains(x._2)).map(_._1)
              assert(tpeOfRemainingGenerics.nonEmpty, s"Remaining generics on ${newFun.symbol.name} not properly instantiated: missing types")
              Apply(TypeApply(newFun, tpeOfRemainingGenerics), args)
            case _ =>
              Apply(newFun, args)
          }
        } else tree
      case fun : Apply =>
        Apply(transformApply(fun), args)
      case _ => tree
    }
  }
}
