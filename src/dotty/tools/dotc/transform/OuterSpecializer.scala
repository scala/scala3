package dotty.tools.dotc.transform

import java.util

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core._
import Decorators._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{ClassSymbol, Symbol}
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.CollectSummaries.SubstituteByParentMap
import dotty.tools.dotc.transform.Summaries.OuterTargs
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.collection.mutable
import scala.reflect.internal.util.Collections

class OuterSpecializer extends MiniPhaseTransform  with InfoTransformer {

  import tpd._

  override def phaseName = "cspec"

  private def primitiveTypes(implicit ctx: Context) =
    defn.ScalaValueTypes

  private def defn(implicit ctx: Context) = ctx.definitions

  type Specialization = Array[Type]

  /**
    * Methods requested for specialization
    * Generic Symbol   =>  List[  (position in list of args, specialized type requested)  ]
    */
  private val specializationRequests: mutable.HashMap[Symbols.Symbol, List[OuterTargs]] = mutable.HashMap.empty

  /**
    * A map that links symbols to their specialized variants.
    * Each symbol maps to another map, from the list of specialization types to the specialized symbol.
    * Generic symbol  =>
    * Map{ List of [ Tuple(position in list of args, specialized Type) ] for each variant  =>  Specialized Symbol }
    */
  private val newSymbolMap: mutable.HashMap[Symbol, mutable.HashMap[OuterTargs, Symbols.Symbol]] = mutable.HashMap.empty

  /**
    * A map that links symbols to their speciazation requests.
    * Each symbol maps to another map, from the list of specialization types to the specialized symbol.
    * Generic symbol  =>
    * Map{ List of [ Tuple(position in list of args, specialized Type) ] for each variant  =>  Specialized Symbol }
    */
  private val outerBySym: mutable.HashMap[Symbol, OuterTargs] = mutable.HashMap.empty

  private val addBridges: mutable.HashMap[ClassSymbol, List[(Symbol, Symbol)]] = mutable.HashMap.empty

  private val originBySpecialized: mutable.HashMap[Symbol, Symbol] = mutable.HashMap.empty

  /**
    * A list of symbols gone through the specialisation pipeline
    * Is used to make calls to transformInfo idempotent
    */
  private val processed: util.IdentityHashMap[Symbol, Symbol] = new util.IdentityHashMap()


  def isSpecializable(sym: Symbol, numOfTypes: Int)(implicit ctx: Context): Boolean =
    numOfTypes > 0 &&
      sym.name != nme.asInstanceOf_ &&
      !newSymbolMap.contains(sym) &&
      !(sym is Flags.JavaDefined) &&
      !sym.isPrimaryConstructor

  /** Get list of types to specialize for */
  def getSpecTypes(method: Symbol, poly: PolyType)(implicit ctx: Context): List[OuterTargs] = {

    val requested = specializationRequests.getOrElse(method, List.empty)
    if (requested.nonEmpty) {
      requested
    }
    else {
      Nil
    }
  }

  /** was decl requested to be specialized */
  def requestedSpecialization(decl: Symbol)(implicit ctx: Context): Boolean = {
    ctx.settings.Yspecialize.value != 0 || specializationRequests.contains(decl) || {
      originBySpecialized.getOrElse(decl, null) match {
        case null => false
        case origin => requestedSpecialization(origin)
      }
    }
  }

  def registerSpecializationRequest(methodOrClass: Symbols.Symbol)(arguments: OuterTargs)
                                   (implicit ctx: Context): Unit = {
    assert(methodOrClass.isClass || methodOrClass.is(Flags.Method))
    if (ctx.phaseId > this.treeTransformPhase.id)
      assert(ctx.phaseId <= this.treeTransformPhase.id)
    val prev = specializationRequests.getOrElse(methodOrClass, List.empty)
    specializationRequests.put(methodOrClass, arguments :: prev)
  }

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = {
    val n = super.transform(ref)
    if (n.symbol.isClass && requestedSpecialization(n.symbol)) {
      val sd = n.asInstanceOf[SymDenotation]
      sd.copySymDenotation(initFlags = sd.flags | Flags.Trait)
    } else n
  }

  /* Provided a class that owns a method to be specialized, adds specializations to the body of the class, without forcing new symbols
  *  provided a method to be specialized, specializes it and enters it into its owner
  * */
  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {

    def enterNewSyms(newDecls: List[Symbol], classInfo: ClassInfo) = {
      val decls = classInfo.decls.cloneScope
      newDecls.foreach(decls.enter)
      classInfo.derivedClassInfo(decls = decls)
    }

    def subsumes(outerTargs1: OuterTargs, outerTargs2: OuterTargs) = {
      true
    }

    def duplicateClass(claz: ClassSymbol, specialization: OuterTargs): ClassSymbol = {
      val mappedParents: List[TypeRef] = claz.classParents.map{parent =>
        newSymbolMap.get(parent.typeSymbol) match {
          case None => parent
          case Some(variants) =>
            variants.find{case (outerTargs, newSym) =>
              subsumes(specialization, outerTargs)
            }.map(_._2.typeRef).getOrElse(parent)
        }
      }
      val newParents = mappedParents.head :: claz.typeRef :: mappedParents.tail
      val map = new SubstituteByParentMap(specialization)
      val newDecls = claz.classInfo.decls.cloneScope.openForMutations // this is a hack. I'm mutating this scope later
      val newType: ClassSymbol => Type = {nwClaz =>
        ClassInfo(claz.classInfo.prefix, nwClaz, newParents, newDecls, claz.classInfo.selfInfo)}

      val newClaz = ctx.newClassSymbol(claz.owner, ctx.freshName(claz.name + "$spec").toTypeName, claz.flags | Flags.Synthetic, newType)

      claz.classInfo.decls.foreach { x =>

        lazy val otherTr = x.typeRef
        lazy val mappedTr = map(otherTr)
        x match {
          case other: Symbol if other.isType && !other.isClass && (otherTr ne mappedTr) =>
            other.info match {
              case tp: TypeBounds =>
                // val newParam = ctx.newSymbol(newClaz, other.name, other.flags, TypeAlias(otherTr), other.privateWithin, other.coord)
                val nw = ctx.newSymbol(newClaz, other.name, other.flags, TypeBounds(tp.lo & mappedTr, tp.hi & mappedTr), other.privateWithin, other.coord)
                newDecls.replace(other, nw)
            }

          case other =>
            val tpe = if (other.isClassConstructor) other.info match {
              case oinfo: PolyType =>
                val newConstructorBounds = claz.info.typeParams.map(x => specialization.mp(claz)(x.name))
                val fullConstructorBounds = (oinfo.paramBounds zip newConstructorBounds).map{case (old, nw) => TypeBounds(old.lo & nw.dropAlias, old.hi & nw.dropAlias)}
                def newResultType(m: MethodType): MethodType =  {
                  m.resultType match {
                    case r: MethodType => m.derivedMethodType(m.paramNames, m.paramTypes, newResultType(r))
                    case r: RefinedType =>
                      m.derivedMethodType(m.paramNames, m.paramTypes, r.translateParameterized(claz, newClaz))
                    case r => ???
                  }
                }
                val resultType = newResultType(oinfo.resultType.asInstanceOf[MethodType])
                oinfo.derivedPolyType(oinfo.paramNames, fullConstructorBounds, resultType)
              case _ => map(other.info)
            } else map(other.info)
            val nw = ctx.newSymbol(newClaz, other.name, other.flags, tpe, other.privateWithin, other.coord)


            if (!other.isClassConstructor && (nw.signature.matchDegree(other.signature) != Signature.FullMatch)) {
              // bridge is needed
              val bridge = ctx.newSymbol(claz, nw.name, nw.flags, nw.info).enteredAfter(this)
              val lst = addBridges.getOrElse(claz, Nil)

              addBridges.put(claz, (bridge, other) :: lst)
            }

            if (other.isTerm && !(other.is(Flags.Method))) {
              other.asSymDenotation.copySymDenotation(initFlags = other.symbol.flags &~ (Flags.Method| Flags.Mutable)).installAfter(this)
            }

            //if(other.isTerm && !other.is(Flags.Method))

            newDecls.replace(other, nw)
        }
      }

      val umap: mutable.HashMap[OuterTargs, Symbols.Symbol] = newSymbolMap.getOrElse(claz, mutable.HashMap.empty)
      umap.put(specialization, newClaz)
      newSymbolMap.put(claz, umap)

      newClaz
    }

    def specializeSymbol(sym: Symbol): Type = {
      processed.put(sym, sym)
      sym.info match {
        case classInfo: ClassInfo =>

          val newDecls = classInfo.decls
            .filter(_.symbol.isCompleted) // We do not want to force symbols. Unforced symbol are not used in the source
            .filterNot(_.isConstructor)
            .filter(requestedSpecialization)
            .flatMap(decl => {
            decl.info.widen match {
              case poly: PolyType if isSpecializable(decl.symbol, poly.paramNames.length) =>
                generateMethodSpecializations(getSpecTypes(decl, poly))(poly, decl)
              case claz: ClassInfo if requestedSpecialization(decl) =>
                specializationRequests(decl).map(x => duplicateClass(decl.asClass, x))
              case _ => Nil
            }
          })

          val ntp =
            if (newDecls.nonEmpty) enterNewSyms(newDecls.toList, classInfo)
            else tp
          ntp
        case poly: PolyType if isSpecializable(sym, poly.paramNames.length) => // specialize method
          if (sym.owner.info.isInstanceOf[ClassInfo]) {
            transformInfo(sym.owner.info, sym.owner)  //why does it ever need to recurse into owner?
            tp
          }
          else if (requestedSpecialization(sym) &&
            isSpecializable(sym, poly.paramNames.length)) {
            generateMethodSpecializations(getSpecTypes(sym, poly))(poly, sym)
            tp
          }
          else tp
        case _ => tp
      }
    }

    def generateMethodSpecializations(specTypes: List[OuterTargs])
                                     (poly: PolyType, decl: Symbol)
                                     (implicit ctx: Context): List[Symbol] = {
     specTypes.map(x => generateSpecializedSymbol(x, poly, decl))
    }

    def generateSpecializedSymbol(instantiations: OuterTargs, poly: PolyType, decl: Symbol)
                                 (implicit ctx: Context): Symbol = {
      val resType = new SubstituteByParentMap(instantiations).apply(poly.resType)

      val bounds = if(instantiations.mp.contains(decl)) (poly.paramBounds zip poly.paramNames).map{case (bound, name) =>
        instantiations.mp.getOrElse(decl, Map.empty).get(name) match {
          case Some(instantiation) => TypeBounds(bound.lo & instantiation, bound.hi & instantiation)
          case None => bound
        }
      } else poly.paramBounds
      val newSym = ctx.newSymbol(
        decl.owner,
        ctx.freshName(decl.name + "$spec").toTermName
        /*NameOps.NameDecorator(decl.name)
          .specializedFor(Nil, Nil, instantiations.toList, poly.paramNames)
          .asInstanceOf[TermName]*/,
        decl.flags | Flags.Synthetic,
        poly.duplicate(poly.paramNames, bounds, resType)
      )

      val map: mutable.HashMap[OuterTargs, Symbols.Symbol] = newSymbolMap.getOrElse(decl, mutable.HashMap.empty)
      map.put(instantiations, newSym)
      newSymbolMap.put(decl, map)
      outerBySym.put(newSym, instantiations)

      newSym
    }

    if (!processed.containsKey(sym) &&
      (sym ne defn.ScalaPredefModule.moduleClass) &&
      !(sym is Flags.JavaDefined) &&
      !(sym is Flags.Scala2x) &&
      !(sym is Flags.Package) &&
      !sym.isAnonymousClass/*why? becasue nobody can call from outside? they can still be called from inside the class*/) {
      specializeSymbol(sym)
    } else tp
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    tree.tpe.widen match {

      case poly: PolyType
        if !(tree.symbol.isPrimaryConstructor
          || (tree.symbol is Flags.Label)
          ) =>

        def specialize(decl : Symbol): List[Tree] = {
          if (newSymbolMap.contains(decl)) {
            val specInfo = newSymbolMap(decl)
            val newSyms = specInfo.values.toList


            newSyms.map { newSym =>
              val newSymType = newSym.info.widenDealias
              println(s"specializing ${tree.symbol} for ${newSymType.show}")
              val typemap:  (Type, List[Symbol], List[Type]) => (List[Symbol], List[Type]) => Type => Type =
                (oldPoly, oldTparams, newTparams) => (oldArgs, newArgs) =>
                  new SubstituteByParentMap(outerBySym(newSym)) {
                    override def apply(tp: Type): Type = {
                      val t = super.apply(tp)
                        .substDealias(oldTparams, newTparams)
                      val t2 = oldPoly match {
                        case oldPoly: PolyType => t.substParams(oldPoly, newTparams)
                        case _ => t
                      }
                      t2.subst(oldArgs, newArgs)
                    }
              }
              duplicateMethod(newSym, tree)(typeMap = typemap)()
            }
          } else Nil
        }
        val specializedTrees = specialize(tree.symbol)
        Thicket(tree :: specializedTrees)
      case _ => tree
    }
  }

  def duplicateMethod(newSym: Symbol, oldTree: DefDef)
                     (typeMap: (Type, List[Symbol], List[Type]) => (List[Symbol], List[Type]) => Type => Type)
                     (substFrom: List[Symbol] = Nil, substTo: List[Symbol] = Nil)
                     (implicit ctx: Context): DefDef = {
    val oldSym = oldTree.symbol
    val origTParams = oldTree.tparams.map(_.symbol)
    val origVParams = oldTree.vparamss.flatten.map(_.symbol)

    polyDefDef(newSym.asTerm, { tparams => vparams => {

      val treemap: (Tree => Tree) = _ match {
        case Return(t, from) if from.symbol == oldSym => Return(t, ref(newSym))
        /* case t: TypeApply =>
           transformTypeApply(t)
         case t: Apply =>
           transformApply(t) */
        case t => t
      }

      val abstractPolyType = oldSym.info.widenDealias
      val vparamTpes = vparams.flatten.map(_.tpe)

      val typesReplaced = new TreeTypeMap(
        treeMap = treemap,
        typeMap = typeMap(abstractPolyType, origTParams, tparams)(origVParams, vparamTpes),
        oldOwners = oldSym :: substFrom,
        newOwners = newSym :: substTo
      ).transform(oldTree.rhs)

      typesReplaced
    }})
  }


  override def transformTypeDef(tree: tpd.TypeDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val oldSym = tree.symbol
    if (oldSym.isClass) newSymbolMap.get(tree.symbol) match {
      case Some(x) =>
        val newClasses: List[Tree] = x.map{case (outersTargs, newSym) =>
          val typemap:  (Type, List[Symbol], List[Type]) => (List[Symbol], List[Type]) => Type => Type =
            (oldPoly, oldTparams, newTparams) => (oldArgs, newArgs) =>
              new SubstituteByParentMap(outersTargs) {
                override def apply(tp: Type): Type = {
                  val t = super.apply(tp)
                    .substDealias(oldTparams, newTparams)
                  val t2 = oldPoly match {
                    case oldPoly: PolyType => t.substParams(oldPoly, newTparams)
                    case _ => t
                  }
                  t2.subst(oldArgs, newArgs)
                }
              }
          //duplicateMethod(newSym, tree)(typeMap = typemap)()
          val treeRhs = tree.rhs.asInstanceOf[Template]
          val newSubst = newSym.info.fields.map(_.symbol).toList// ++ newSym.info.accessors
          val oldSubst = oldSym.info.fields.map(_.symbol).toList// ++ oldSym.info.accessors
          val bodytreeTypeMap = new TreeTypeMap(typeMap = typemap(null, Nil, Nil)(Nil, Nil), substFrom = oldSubst, substTo = newSubst
            /*oldOwners = oldSubst, newOwners = newSubst*/)
          val constr = duplicateMethod(newSym.primaryConstructor, treeRhs.constr)(typemap)(oldSubst, newSubst)

          val body = treeRhs.body.map{original =>
             original match {
               case t: DefDef =>
                 val newMeth: Symbol = // todo: handle overloading
                   /*if(!t.symbol.is(Flags.Private)) t.symbol.matchingMember(newSym.info) // does not work. Signatures do not match anymore
                   else */newSym.info.decl(t.symbol.name).asSymDenotation.symbol

                 duplicateMethod(newMeth, t)(typemap)(oldSubst, newSubst)
               case t: TypeDef if !t.isClassDef =>
                 val newMember = newSym.info.decl(t.symbol.name).asSymDenotation.symbol.asType
                 tpd.TypeDef(newMember)
               case t: ValDef =>
                 val newMember = newSym.info.decl(t.symbol.name).asSymDenotation.symbol.asTerm
                 tpd.ValDef(newMember, bodytreeTypeMap.apply(t.rhs))
               case _ => // just body. TTM this shit
                 bodytreeTypeMap.apply(original)
             }
          }
          val superArgs = treeRhs.parents.head match {
            case Apply(fn, args) => args
            case _ => Nil
          }

          tpd.ClassDef(newSym.asClass, constr, body, superArgs)
        }.toList

        val newBridges = {
          val bridgeSymbols = addBridges(tree.symbol.asClass)
          bridgeSymbols.map{case(nw, old) =>
             polyDefDef(nw.asTerm, tparams => vparamss => {

               val prefix = This(oldSym.asClass).select(old).appliedToTypes(tparams)
               val argTypess = prefix.tpe.widen.paramTypess
               val argss = Collections.map2(vparamss, argTypess){(vparams,  argTypes) =>
                Collections.map2(vparams, argTypes){(vparam, argType) => vparam.ensureConforms(argType)}
               }
               prefix.appliedToArgss(argss).ensureConforms(nw.info.finalResultType)
             })
          }

        }

        val newTrait =
          if (newBridges.nonEmpty){
            val currentRhs = tree.rhs.asInstanceOf[Template]
            cpy.TypeDef(tree)(rhs = cpy.Template(currentRhs)(body = currentRhs.body ++ newBridges))
          } else tree

        Thicket(newTrait :: newClasses)
      case None => tree
    } else tree
  }

  def rewireTree(tree: Tree)(implicit ctx: Context): Tree = {
    assert(tree.isInstanceOf[TypeApply])
    val TypeApply(fun, args) = tree

    if (fun.symbol.isPrimaryConstructor && newSymbolMap.contains(fun.symbol.owner)) {
      val availableSpecializations = newSymbolMap(fun.symbol.owner)
      val poly = fun.symbol.info.widen.asInstanceOf[PolyType]
      val argsNames = fun.symbol.owner.asClass.classInfo.typeParams.map(_.name) zip args
      val betterClasses = availableSpecializations.filter {
        case (instantiations, symbol) => {
          val mappings = instantiations.mp(fun.symbol.owner)
          argsNames.forall { case (name, arg) => arg.tpe <:< mappings(name).dropAlias }
        }
      }.toList

      if (betterClasses.length > 1) {
        ctx.debuglog(s"Several specialized variants fit for ${fun.symbol.name} of ${fun.symbol.owner}." +
          s" Defaulting to no specialization.")
        tree
      }

      else if (betterClasses.nonEmpty) {
        val newClassSym = betterClasses.head._2
        ctx.debuglog(s"new ${fun.symbol.owner} rewired to ${newClassSym}")
        tpd.New(newClassSym.typeRef)
          .select(newClassSym.primaryConstructor)  // todo handle secondary cosntr
          .appliedToTypeTrees(args)
      } else tree

    } else if (newSymbolMap.contains(fun.symbol)) {
      val poly = fun.symbol.info.widen.asInstanceOf[PolyType]
      val argsNames = poly.paramNames zip args
      val availableSpecializations = newSymbolMap(fun.symbol)
      val betterDefs = availableSpecializations.filter {
        case (instantiations, symbol) => {
          val mappings = instantiations.mp(fun.symbol)
          argsNames.forall { case (name, arg) => arg.tpe <:< mappings(name) }
        }
      }.toList

      if (betterDefs.length > 1) {
        ctx.debuglog(s"Several specialized variants fit for ${fun.symbol.name} of ${fun.symbol.owner}." +
          s" Defaulting to no specialization.")
        tree
      }

      else if (betterDefs.nonEmpty) {
        val newFunSym = betterDefs.head._2
        ctx.debuglog(s"method ${fun.symbol.name} of ${fun.symbol.owner} rewired to specialized variant")
        val prefix = fun match {
          case Select(pre, name) =>
            pre
          case t@Ident(_) if t.tpe.isInstanceOf[TermRef] =>
            val tp = t.tpe.asInstanceOf[TermRef]
            if (tp.prefix ne NoPrefix)
              ref(tp.prefix.termSymbol)
            else EmptyTree
          case _ => EmptyTree
        }
        if (prefix ne EmptyTree) prefix.select(newFunSym).appliedToTypeTrees(args)
        else ref(newFunSym).appliedToTypeTrees(args)
      } else tree
    } else tree

  }

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val TypeApply(fun, _) = tree
    if (fun.tpe.widenDealias.isParameterless) rewireTree(tree)
    else tree
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val Apply(fun, args) = tree
    fun match {
      case fun: TypeApply =>
        val typeArgs = fun.args
        val newFun = rewireTree(fun)
        if (fun ne newFun)
          Apply(newFun, args)
        else tree
      case fun : Apply =>
        Apply(transformApply(fun), args)
      case _ => tree
    }
  }
}
