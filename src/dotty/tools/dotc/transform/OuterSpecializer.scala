package dotty.tools.dotc.transform

import java.util

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Names.{Name, TermName, TypeName}
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core._
import Decorators._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Flags.FlagSet
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{ClassSymbol, Symbol}
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.CollectSummaries.SubstituteByParentMap
import dotty.tools.dotc.transform.Summaries.OuterTargs
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

import scala.collection.mutable
import scala.reflect.internal.util.Collections


// TODO: Check secondary constructors.
// TODO: check private fields
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
  val newSymbolMap: mutable.HashMap[Symbol, mutable.HashMap[OuterTargs, Symbols.Symbol]] = mutable.HashMap.empty

  /**
   * A map that links symbols to their speciazation requests.
   * Each symbol maps to another map, from the list of specialization types to the specialized symbol.
   * Generic symbol  =>
   * Map{ List of [ Tuple(position in list of args, specialized Type) ] for each variant  =>  Specialized Symbol }
   */
  val outerBySym: mutable.HashMap[Symbol, OuterTargs] = mutable.HashMap.empty

  val addBridges: mutable.HashMap[ClassSymbol, List[(Symbol, Symbol)]] = mutable.HashMap.empty

  val originBySpecialized: mutable.HashMap[Symbol, Symbol] = mutable.HashMap.empty

  /**
   * A list of symbols gone through the specialisation pipeline
   * Is used to make calls to transformInfo idempotent
   */
  private val processed: util.IdentityHashMap[Symbol, Type] = new util.IdentityHashMap()


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
        case origin if !origin.isClass => requestedSpecialization(origin) // a specialized version of specialized method todo: Am i right?
        case _ => false
      }
    }
  }

  def isSimilar(arguments: OuterTargs, other: OuterTargs)(implicit ctx: Context) = {
    other.mp.forall { other =>
      arguments.mp.get(other._1) match {
        case None => false
        case Some(mapping) =>
          mapping.forall { thisMapping =>
            other._2.get(thisMapping._1) match {
              case None => false
              case Some(otherTp) =>
                otherTp.dropAlias =:= thisMapping._2.dropAlias
            }
          }
      }
    }
  }

  def subsumes(arguments: OuterTargs, other: OuterTargs)(implicit ctx: Context) = {
    other.mp.forall { other =>
      arguments.mp.get(other._1) match {
        case None => false
        case Some(mapping) =>
          mapping.forall { thisMapping =>
            other._2.get(thisMapping._1) match {
              case None => false
              case Some(otherTp) =>
                otherTp.dropAlias <:< thisMapping._2.dropAlias
            }
          }
      }
    }
  }

  def registerSpecializationRequest(methodOrClass: Symbols.Symbol)(arguments: OuterTargs)
                                   (implicit ctx: Context): Unit = {
    if (((methodOrClass.isClass && (!methodOrClass.is(Flags.Module) || !methodOrClass.isStatic)) ||
           methodOrClass.is(Flags.Method))
      && (methodOrClass.sourceFile ne null))  {
      if (methodOrClass.name.toString == "map" && methodOrClass.owner.name.toString == "Iterator")
        println("here")
      if (ctx.phaseId > this.treeTransformPhase.id)
        assert(ctx.phaseId <= this.treeTransformPhase.id)
      val prev = specializationRequests.getOrElse(methodOrClass, List.empty)
      def isSimilar(arguments: OuterTargs, other: OuterTargs) = {
        other.mp.forall { other =>
          arguments.mp.get(other._1) match {
            case None => false
            case Some(mapping) =>
              mapping.forall { thisMapping =>
                other._2.get(thisMapping._1) match {
                  case None => false
                  case Some(otherTp) =>
                    otherTp =:= thisMapping._2
                }
              }
          }
        }
      }
      if (prev.exists(isSimilar(arguments, _)))
        return;
      specializationRequests.put(methodOrClass, arguments :: prev)
    }
    else {
      println(s"ignoring specialization reguest for ${methodOrClass.showFullName} for ${arguments}")
    }
  }

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = {
    val n = super.transform(ref)
    if (n.symbol.isClass && requestedSpecialization(n.symbol)) {
      val sd = n.asInstanceOf[SymDenotation]
      val newParent = n.symbol.asClass.baseClasses.find(x => x.isClass && !(x is Flags.Trait) && !requestedSpecialization(x))
      val info = n.info.asInstanceOf[ClassInfo]
      // todo: fix parents
      sd.copySymDenotation(initFlags = sd.flags | Flags.Trait, info = info)
    } else n
  }

  /* Provided a class that owns a method to be specialized, adds specializations to the body of the class, without forcing new symbols
  *  provided a method to be specialized, specializes it and enters it into its owner
  * */
  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {

    def enterNewSyms(newDecls: List[Symbol], classInfo: ClassInfo, tp: Type) = {
      if (!classInfo.typeSymbol.is(Flags.Package)) {
        val decls = classInfo.decls.cloneScope
        newDecls.foreach(decls.enter)
        classInfo.derivedClassInfo(decls = decls)
      } else {
        newDecls.foreach(_.entered)
        classInfo // used to return tp here. why?
      }
    }

    def duplicateClass(originalClass: ClassInfo, specialization: OuterTargs): ClassSymbol = {
      val claz = originalClass.typeSymbol.asClass

      val newParents = originalClass.classParents.head :: claz.typeRef :: originalClass.classParents.tail
      val map = new SubstituteByParentMap(specialization)
      val newDecls = originalClass.decls.cloneScope.openForMutations // this is a hack. I'm mutating this scope later
      val newType: ClassSymbol => Type = { nwClaz =>
        ClassInfo(originalClass.prefix, nwClaz, newParents, newDecls, map.apply(originalClass.selfType))
      }

      def fixModule(nm: TypeName): TypeName = {
        import NameOps._
        if (claz.flags is Flags.Module) nm.moduleClassName
        else nm
      }
      val newClaz = ctx.newClassSymbol(claz.owner, fixModule(ctx.freshName(claz.name + "$spec").toTypeName), claz.flags | Flags.Synthetic, newType)

      originalClass.decls.foreach { originalDecl =>

        lazy val otherTr = originalDecl.typeRef
        lazy val mappedTr = map(otherTr)
        originalDecl match {
          case other: Symbol if other.isType && !other.isClass && (otherTr ne mappedTr) =>
            other.info match {
              case tp: TypeBounds =>
                // val newParam = ctx.newSymbol(newClaz, other.name, other.flags, TypeAlias(otherTr), other.privateWithin, other.coord)
                val newBoulds =
                  if (mappedTr.typeSymbol eq defn.AnyClass) tp
                  else TypeAlias(mappedTr)
                val nw = ctx.newSymbol(newClaz, other.name, other.flags, newBoulds, other.privateWithin, other.coord)
                newDecls.replace(other, nw)
            }

          case other =>
            if (other.name.toString == "next")
              println("bla")
            val tpe = if (other.isClassConstructor) other.info match {
              case oinfo: PolyType =>
                val newConstructorBounds = originalClass.typeParams.map(x => specialization.mp(claz)(x.name))
                val fullConstructorBounds = (oinfo.paramBounds zip newConstructorBounds).map { case (old, nw) => TypeBounds(old.lo | nw.dropAlias, old.hi & nw.dropAlias) }
                def newResultType(m: MethodType): MethodType = {
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
            def fixMethodic(tp: Type, flags: FlagSet) = {
              if (flags is Flags.Method)
                if (tp.isInstanceOf[MethodicType]) tp
                else ExprType(tp)
              else tp
            }
            val nw = ctx.newSymbol(newClaz, other.name, other.flags, fixMethodic(tpe, other.flags), other.privateWithin, other.coord)
            originBySpecialized.put(nw, other)

            if (nw.name.toString.contains("import"))
              println("hoo")

            val currentBridge = addBridges.getOrElse(claz, Nil).filter(x => x._2 == other && x._1.signature == nw.signature)


            if (!other.isClassConstructor && (nw.signature.matchDegree(other.signature) != Signature.FullMatch) && currentBridge.isEmpty) {
              // bridge is needed

              {
                val bridgeInSuper = ctx.newSymbol(claz, nw.name, nw.flags.&~(Flags.Accessor | Flags.ParamAccessor) | Flags.Bridge, nw.info).enteredAfter(this)
                val lst = addBridges.getOrElse(claz, Nil)
                addBridges.put(claz, (bridgeInSuper, other) :: lst)
              }

              {
                val bridgeInSub =
                  ctx.newSymbol(newClaz, nw.name, nw.flags.&~(Flags.Accessor | Flags.ParamAccessor) | Flags.Bridge, other.info)
                val lst = addBridges.getOrElse(newClaz, Nil)

                newDecls.enter(bridgeInSub)
                addBridges.put(newClaz, (bridgeInSub, nw) :: lst)
              }


            }

            if (other.isTerm && !(other.is(Flags.Method))) {
              // field
              val newFlags = (other.symbol.flags &~ (Flags.Mutable)) | Flags.Deferred | Flags.Method | Flags.Stable
              other.asSymDenotation.copySymDenotation(initFlags = newFlags, info = fixMethodic(other.info, newFlags)).installAfter(this)
            }

            /* if (other.isTerm && other.is(Flags.ParamAccessor)) {
              val newFlags = (other.symbol.flags &~ (Flags.ParamAccessor | Flags.Accessor | Flags.Private)) | Flags.Deferred | Flags.Method| Flags.Stable
              other.asSymDenotation.copySymDenotation(initFlags = newFlags, info = fixMethodic(other.info, newFlags)).installAfter(this)
            } */

            newDecls.replace(other, nw)
        }
      }

      val umap: mutable.HashMap[OuterTargs, Symbols.Symbol] = newSymbolMap.getOrElse(claz, mutable.HashMap.empty)
      umap.put(specialization, newClaz)
      newSymbolMap.put(claz, umap)
      originBySpecialized.put(newClaz, claz)

      newClaz
    }

    def specializeSymbol(sym: Symbol): Type = {
      if (sym.name.toString.contains("shortWrapper"))
        println("hdd")
      processed.put(sym, NoType)
      ctx.debuglog(s"started specializing type of $sym")
      val ret = sym.info match {
        case classInfo: ClassInfo =>

          val newDecls = classInfo.decls
            .filter(x => x.isDefinedInCurrentRun && x.isCompleted) // We do not want to force symbols. Unforced symbol are not used in the source
            .filterNot(_.isConstructor)
            .filter(requestedSpecialization)
            .flatMap(decl => {
              decl.info.widen match {
                case poly: PolyType if isSpecializable(decl.symbol, poly.paramNames.length) =>
                  generateMethodSpecializations(getSpecTypes(decl, poly))(poly, decl)
                case claz: ClassInfo if requestedSpecialization(decl) =>
                  def addGenericSpec(x: List[OuterTargs]): List[OuterTargs] =
                    if (x.isEmpty) x
                    else {
                      val tparams = decl.typeParams
                      val generic = x.find(_.mp.values.flatten.forall(x => TypeErasure.erasure(x._2) == defn.ObjectType))
                      if (generic.isEmpty) {
                        val tparamsMapping: Map[Name, Type] = tparams.map(x => (x.name, TypeAlias(defn.AnyType))).toMap
                        OuterTargs(Map(decl -> tparamsMapping)) :: x
                      } else x
                    }
                  val clazInfo = specializeSymbol(decl.asClass).asInstanceOf[ClassInfo]
                  /*addGenericSpec*/ (specializationRequests(decl)).map(x => duplicateClass(clazInfo, x))
                case _ => Nil
              }
            })

          val ntp =
            if (newDecls.nonEmpty) enterNewSyms(newDecls.toList, classInfo, tp)
            else classInfo
          ntp
        case poly: PolyType if isSpecializable(sym, poly.paramNames.length) => // specialize method
          if (sym.owner.info.isInstanceOf[ClassInfo]) {
            transformInfo(sym.owner.info, sym.owner) //why does it ever need to recurse into owner?
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
      processed.put(sym, ret)
      ctx.debuglog(s"finished specializing $sym")
      ret
    }

    def generateMethodSpecializations(specTypes: List[OuterTargs])
                                     (poly: PolyType, decl: Symbol)
                                     (implicit ctx: Context): List[Symbol] = {
      specTypes.map(x => generateSpecializedSymbol(x, poly, decl))
    }

    def generateSpecializedSymbol(instantiations: OuterTargs, poly: PolyType, decl: Symbol)
                                 (implicit ctx: Context): Symbol = {
      val resType = new SubstituteByParentMap(instantiations).apply(poly.resType)

      val bounds = if (instantiations.mp.contains(decl)) (poly.paramBounds zip poly.paramNames).map { case (bound, name) =>
        instantiations.mp.getOrElse(decl, Map.empty).get(name) match {
          case Some(instantiation) => TypeBounds(bound.lo | instantiation, bound.hi & instantiation)
          case None => bound
        }
      } else poly.paramBounds
      val newSym = ctx.newSymbol(
        decl.owner,
        ctx.freshName(decl.name + "$spec").toTermName
        /*NameOps.NameDecorator(decl.name)
          .specializedFor(Nil, Nil, instantiations.toList, poly.paramNames)
          .asInstanceOf[TermName]*/ ,
        decl.flags | Flags.Synthetic,
        poly.duplicate(poly.paramNames, bounds, resType)
      )

      val map: mutable.HashMap[OuterTargs, Symbols.Symbol] = newSymbolMap.getOrElse(decl, mutable.HashMap.empty)
      map.put(instantiations, newSym)
      newSymbolMap.put(decl, map)
      outerBySym.put(newSym, instantiations)

      newSym
    }

    if (processed.containsKey(sym)) {
      val v = processed.get(sym)
      if (v eq NoType)
        ctx.error("circular error")
      v
    }

    if (!processed.containsKey(sym) &&
      (sym ne defn.ScalaPredefModule.moduleClass) &&
      !(sym is(Flags.JavaDefined, Flags.Package)) &&
      !(sym is Flags.Scala2x) &&
      !sym.isAnonymousClass /*why? becasue nobody can call from outside? they can still be called from inside the class*/ ) {
      specializeSymbol(sym)
    } else tp
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    tree.tpe.widen match {

      case poly: PolyType
        if !(tree.symbol.isPrimaryConstructor
          || (tree.symbol is Flags.Label)
          ) =>

        def specialize(decl: Symbol): List[Tree] = {
          if (newSymbolMap.contains(decl)) {
            val specInfo = newSymbolMap(decl)
            val newSyms = specInfo.values.toList


            newSyms.map { newSym =>
              val newSymType = newSym.info.widenDealias
              println(s"specializing ${tree.symbol.fullName} for ${newSymType.show}")
              val typemap: (Type, List[Symbol], List[Type]) => (List[Symbol], List[Type]) => Type => Type =
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
    if (newSym.name.toString.contains("IterablePolyTransforms$spec2"))
      println("hsdds")
    val oldSym = oldTree.symbol
    originBySpecialized.put(newSym, oldSym)
    val origTParams = oldTree.tparams.map(_.symbol)
    val origVParams = oldTree.vparamss.flatten.map(_.symbol)


    polyDefDef(newSym.asTerm, { tparams => vparams => {

      val treemap: (Tree => Tree) = _ match {
        case Return(t, from) if from.symbol == oldSym => Return(t, ref(newSym))
        case t: This if t.symbol eq oldSym.enclosingClass => This(newSym.enclosingClass.asClass)
        case t => t
      }

      val abstractPolyType = oldSym.info.widenDealias
      val vparamTpes = vparams.flatten.map(_.tpe)

      if (oldTree.uniqueId == 15973)
        println("catch")

      val typesReplaced = new TreeTypeMap(
        treeMap = treemap,
        typeMap = typeMap(abstractPolyType, origTParams, tparams)(origVParams, vparamTpes),
        oldOwners = oldSym :: substFrom,
        newOwners = newSym :: substTo
      ).transform(oldTree.rhs)

      typesReplaced
    }
    })
  }


  override def transformTypeDef(tree: tpd.TypeDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val oldSym = tree.symbol
    if (oldSym.isClass) newSymbolMap.get(tree.symbol) match {
      case Some(x) =>
        val newClasses: List[Tree] = x.map { case (outersTargs, newClassSym) =>
          val typemap: (Type, List[Symbol], List[Type]) => (List[Symbol], List[Type]) => Type => Type =
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
          val newSubst = newClassSym.info.fields.map(_.symbol).toList // ++ newSym.info.accessors
        val oldSubst = oldSym.info.fields.map(_.symbol).toList // ++ oldSym.info.accessors
        val treeMap: Tree => Tree = {
          case t: This if (t.symbol eq oldSym) => tpd.This(newClassSym.asClass)
          case t => t
        }
          val bodytreeTypeMap = new TreeTypeMap(typeMap = typemap(null, Nil, Nil)(Nil, Nil), substFrom = oldSubst, substTo = newSubst, treeMap = treeMap
            /*oldOwners = oldSubst, newOwners = newSubst*/)
          val constr = duplicateMethod(newClassSym.primaryConstructor, treeRhs.constr)(typemap)(oldSubst, newSubst)

          val body = treeRhs.body.map { original =>
            original match {
              case t: DefDef =>

                val variants = newClassSym.info.decl(t.symbol.name).suchThat(p => !(p is Flags.Bridge))
                val popa = variants.alternatives.map(_.symbol.info.overrides(original.symbol.info))
                val newMeth: Symbol = // todo: handle overloading
                /*if(!t.symbol.is(Flags.Private)) t.symbol.matchingMember(newSym.info) // does not work. Signatures do not match anymore
                   else */
                  t.symbol.asSymDenotation.matchingDecl(newClassSym, newClassSym.thisType).orElse(
                    variants.symbol
                  )
                //x.matchingDecl(original.symbol.owner.asClass, x.owner.thisType).exists)

                duplicateMethod(newMeth, t)(typemap)(oldSubst, newSubst)
              case t: TypeDef if !t.isClassDef =>
                val newMember = newClassSym.info.decl(t.symbol.name).asSymDenotation.symbol.asType
                tpd.TypeDef(newMember)
              case t: ValDef =>
                if (!newClassSym.info.decl(t.symbol.name).asSymDenotation.exists)
                  println("wtf")
                val newMember = newClassSym.info.decl(t.symbol.name).asSymDenotation.symbol.asTerm
                tpd.ValDef(newMember, bodytreeTypeMap.apply(t.rhs))
              case _ => // just body. TTM this shit
                bodytreeTypeMap.apply(original)
            }
          }
          val superArgs = treeRhs.parents.head match {
            case Apply(fn, args) => args
            case _ => Nil
          }

          tpd.ClassDef(newClassSym.asClass, constr, body, superArgs)
        }.toList

        val newBridges = {
          val bridgeSymbols = addBridges.getOrElse(tree.symbol.asClass, Nil)
          if (bridgeSymbols.isEmpty)
            println(s"generating no bridges for ${tree.symbol}")
          bridgeSymbols.map { case (nw, old) =>
            polyDefDef(nw.asTerm, tparams => vparamss => {

              val prefix = This(oldSym.asClass).select(old).appliedToTypes(tparams)
              val argTypess = prefix.tpe.widen.paramTypess
              val argss = Collections.map2(vparamss, argTypess) { (vparams, argTypes) =>
                Collections.map2(vparams, argTypes) { (vparam, argType) => vparam.ensureConforms(argType) }
              }
              prefix.appliedToArgss(argss).ensureConforms(nw.info.finalResultType)
            })
          }

        }

        val newTrait =
          if (newBridges.nonEmpty) {
            val currentRhs = tree.rhs.asInstanceOf[Template]
            cpy.TypeDef(tree)(rhs = cpy.Template(currentRhs)(body = currentRhs.body ++ newBridges))
          } else tree

        val ret = Thicket(newTrait :: newClasses)
        ret
      case None =>
        tree
    } else tree
  }

  def rewireTree(tree: Tree)(implicit ctx: Context): Tree = {
    assert(tree.isInstanceOf[TypeApply])
    val TypeApply(fun, args) = tree

    if (fun.symbol.isPrimaryConstructor && fun.symbol.owner.name.toString.contains("ArrayBuf"))
      println("here")


    if (fun.symbol.isPrimaryConstructor && newSymbolMap.contains(fun.symbol.owner)) {
      val availableSpecializations = newSymbolMap(fun.symbol.owner)
      val poly = fun.symbol.info.widen.asInstanceOf[PolyType]
      val argsNames = fun.symbol.owner.asClass.classInfo.typeParams.map(_.name) zip args
      val availableClasses = availableSpecializations.filter {
        case (instantiations, symbol) => {
          val mappings = instantiations.mp(fun.symbol.owner)
          argsNames.forall { case (name, arg) => arg.tpe <:< mappings(name).dropAlias }
        }
      }

      val bestVersions = availableClasses.filter { case (instantiations1, symbol1) =>
        availableClasses.find { case (instantiations2, symbol2) =>
          (symbol2 ne symbol1) && subsumes(instantiations1, instantiations2)
        }.isEmpty
      }.toList

      val ideal = availableSpecializations.find {
        case (instantiations, symbol) => {
          val mappings = instantiations.mp(fun.symbol.owner)
          argsNames.forall { case (name, arg) => arg.tpe =:= mappings(name).dropAlias }
        }
      }

      def rewrite(newClassSym: Symbol) = {
        ctx.debuglog(s"new ${fun.symbol.owner} rewired to ${newClassSym}")
        tpd.New(newClassSym.typeRef)
          .select(newClassSym.primaryConstructor) // todo handle secondary cosntr
          .appliedToTypeTrees(args)
      }

      if (ideal.nonEmpty) {
        rewrite(ideal.get._2)
      } else if (bestVersions.length > 1) {
        ctx.error(s"Several specialized variants fit for ${fun.symbol.name} of ${fun.symbol.owner}." +
          s" Defaulting to no specialization. This is not supported yet. Variants: \n ${bestVersions.map { x => (x._2.name, x._2.info.widenDealias.show) }.mkString("\n")}")
        tree
      } else if (bestVersions.nonEmpty) {
        val newClassSym = bestVersions.head._2
        ctx.debuglog(s"new ${fun.symbol.owner} rewired to ${newClassSym}")
        tpd.New(newClassSym.typeRef)
          .select(newClassSym.primaryConstructor) // todo handle secondary cosntr
          .appliedToTypeTrees(args)
      } else EmptyTree
    } else if (newSymbolMap.contains(fun.symbol)) {
      // not a constructor
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
        ctx.debuglog(s"Several specialized variants fit for ${fun.symbol.name} of ${fun.symbol.owner}.")
      }

      if (betterDefs.nonEmpty) {
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

  def transormGenApply(tree: GenericApply[Type])(implicit ctx: Context): Tree = {
    if (ctx.owner.name.toString.contains("next"))
      println("heere")
    tree match {
      case t: tpd.TypeApply =>
        val TypeApply(fun, _) = tree
        if (fun.tpe.widenDealias.isParameterless) rewireTree(tree)
        else tree
      case t: tpd.Apply =>
        val Apply(fun, args) = tree
        fun match {
          case fun: TypeApply =>
            val typeArgs = fun.args
            val newFun = rewireTree(fun)
            if (fun ne newFun)
              if (newFun ne EmptyTree)
                Apply(newFun, args)
              else {
                Typed(ref(defn.Sys_errorR).appliedTo(Literal(Constant("should never be reached"))), TypeTree(tree.tpe))
              }
            else tree
          case fun: Apply =>
            Apply(transormGenApply(fun), args)
          case _ => tree
        }
    }
  }
}

class OuterSpecializeParents extends MiniPhaseTransform with InfoTransformer{
  var specPhase: OuterSpecializer = null

  override def init(base: ContextBase, id: Int): Unit = {
    specPhase = base.phaseOfClass(classOf[OuterSpecializer]).asInstanceOf[OuterSpecializer]
    super.init(base, id)
  }

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
    if (sym.isClass && specPhase.originBySpecialized.contains(sym)) {
      val origSym = specPhase.originBySpecialized(sym)
      if (origSym.symbol.name.toString.contains("IterablePolyTransforms"))
        println("dd")
      val specialization: OuterTargs = specPhase.newSymbolMap(origSym).find(_._2 == sym).get._1

      tp match {
        case classInfo: ClassInfo =>
          val List(classParent, original, others @ _*) = classInfo.classParents

          def betterParent (parent: TypeRef): TypeRef = {
            specPhase.newSymbolMap.get(parent.typeSymbol) match {
              case None => parent
              case Some(variants) =>
                /* same as in CollectSummaries.sendSpecializationRequests */
                def filterApplies(spec: OuterTargs, parent: Symbol) = {
                 OuterTargs(spec.mp.filter{x => parent.derivesFrom(x._1)})
                }

                variants.find { case (outerTargs, newSym) =>
                  specPhase.isSimilar(filterApplies(specialization, parent.typeSymbol), filterApplies(outerTargs, parent.typeSymbol))
                }.map(_._2.typeRef).getOrElse(parent)
            }
          }
          val mappedParents: List[TypeRef] = betterParent(classParent) :: original :: others.map(betterParent).toList
          if (mappedParents.map(_.typeSymbol).contains(sym))
            println("fail")
          classInfo.derivedClassInfo(classParents = mappedParents)
        case _ =>
          ???
      }
    } else tp
  }

  val phaseName: String = "cspec-parents"

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = specPhase.transormGenApply(tree)

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = specPhase.transormGenApply(tree)
}
