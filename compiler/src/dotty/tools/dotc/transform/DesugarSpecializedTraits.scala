package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Decorators.className
import dotty.tools.dotc.core.Symbols.{Symbol, ClassSymbol, newNormalizedClassSymbol}
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.newClassSymbol
import scala.Function.const
import dotty.tools.dotc.core.Names.TypeName
import dotty.tools.dotc.core.Symbols.TypeSymbol
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.ast.TreeTypeMap
import dotty.tools.dotc.core.Scopes.EmptyScope
import dotty.tools.dotc.core.StdNames.str.SPECIALIZED_TRAIT_SUFFIX
import dotty.tools.dotc.core.Names.Name
import tpd._
import scala.collection.mutable
import scala.annotation.unspecialized
import dotty.tools.dotc.typer.Synthesizer
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Flags.GivenOrImplicit
import dotty.tools.dotc.core.NameKinds.ContextBoundParamName
import dotty.tools.dotc.core.NameKinds.FlatName
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.report
import dotty.tools.dotc.core.Flags.InlineTrait
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.util.SrcPos
import dotty.tools.dotc.core.Decorators.nestedMap
import dotty.tools.dotc.core.NameOps.expandedName
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags.InlineMethod
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.util.Spans.spanCoord
import dotty.tools.dotc.util.Spans.NoSpan
import dotty.tools.dotc.transform.DesugarSpecializedTraits.specializationOf
import dotty.tools.dotc.transform.DesugarSpecializedTraits.isTopClass
import dotty.tools.dotc.reporting.VarianceInSpecializedTraitsLimitation
import dotty.tools.dotc.transform.DesugarSpecializedTraits.isTopClassOrNothing
import dotty.tools.dotc.inlines.Inlines.InlineTraitState
import dotty.tools.dotc.core.Decorators.em

class DesugarSpecializedTraits extends MiniPhase, IdentityDenotTransformer:

  override def phaseName: String = DesugarSpecializedTraits.name
  override def description: String = DesugarSpecializedTraits.description
  override def changesMembers: Boolean = false
  override def changesParents: Boolean = true 

  override def runsAfter: Set[String] = Set("checkInlineTraits")

  override def allowsImplicitSearch: Boolean = true

  private def newInterfaceTrait(specialization: Specialization, cache: SpecializationCache)(using Context): (ClassSymbol, SpecializationCache) = {
    val tm = specialization.getTypeMap
    val inheritedParents = specialization.symbol.denot.info.parents.filterNot(_.typeSymbol == defn.ObjectClass).map(tm(_))
    // Parents may be specializable and so we need to specialize them as well
    // See ArrayIterator extends Iterator in specialized-trait-collections-example.scala
    val specializations1 = inheritedParents.foldLeft(cache)((specializations, parent) => 
        (parent, specialization.span) match {
          case Specialization(spec) if spec.isSpecialized => specializations.addInterface(spec) 
          case _ => specializations
        }
    )

    // Order is depended on in Erasure::typedClassDef and TypeErasure:eraseParent
    val parents = defn.ObjectType
                  // original trait, specialized to Foo[Int]
                  :: AppliedType(specialization.symbol.typeRef, specialization.specialization)
                  // parents of the original trait in the form Foo[Int] (later specialized to Foo$sp$Int)
                  :: inheritedParents                             

    // Create new trait
    val traitSymbol = newNormalizedClassSymbol(
      // For specialized traits defined inside objects/classes etc, pre-Flatten the $sp$ and $impl$ def trees (i.e.
      // make them live in the enclosing package with the flattened name). We do this because it's easier than 
      // finding the defining tree of the object, which would require scanning the whole file, and it
      // might be in another compilation unit / already compiled.
      specialization.symbol.owner.enclosingPackageClass, 
      specialization.newSpecializedTraitName,
      Flags.Synthetic | Flags.Trait | Flags.Inline,
      parents,
      tm(specialization.symbol.asClass.classInfo.selfType),
      specialization.symbol.privateWithin,
      // TODO: Show errors where they actually show up in the inline trait and not at the type of the user 
      // which is very confusing. Need to use the inline stack (i.e. "this location contains code inlined from ...").
      // In particular if the user does not specify the type explicitly but a specialization of an inline trait 
      // is the inferred type, the error cursor can point to somewhere very unexpected as the type
      // does not appear in the source code. Probably need to change some spans and coords in other places as well.
      spanCoord(specialization.span), 
      specialization.symbol.compilationUnitInfo
    )

    buildTypeParameters(traitSymbol, specialization)
    (traitSymbol.entered, specializations1)
  }

  private def buildInterfaceTraitTree(specialization: Specialization, interfaceSymbol: ClassSymbol)(using Context) = {
    val init = newConstructor(interfaceSymbol, EmptyFlags, Nil, Nil, coord=spanCoord(specialization.span))
    fixConstructor(init, interfaceSymbol)
    ClassDef(interfaceSymbol, DefDef(init.entered), Nil).withSpan(specialization.span)
  }

  /* Fix constructor so that it:
      1) Has correct generic type parameters
      2) Returns the correct type corresponding to those type parameters applied 
      3) Has correct parameter names corresponding to targetParamNames */
  private def fixConstructor(init: Symbol, cls: ClassSymbol, params: List[List[TermName]] = Nil)(using Context) = 
    val rt = cls.typeRef.appliedTo(cls.typeParams.map(_.typeRef))
    def resultType(tpe: Type, params: List[List[TermName]]): Option[Type] = 
      tpe match {
        case mt @ MethodType(paramNames) => params match {
          case head :: tail => 
            Some(mt.derivedLambdaType(head, mt.paramInfos, resultType(mt.resultType, tail).getOrElse(rt)))
          case Nil          => 
            Some(mt.derivedLambdaType(paramNames, mt.paramInfos, resultType(mt.resultType, params).getOrElse(rt)))
        }
        case pt : PolyType => 
          Some(pt.derivedLambdaType(pt.paramNames, pt.paramInfos, resultType(pt.resType, params).get))
        case _ => None
    }
    init.info = resultType(init.info, params).get
    init.info = PolyType.fromParams(init.owner.typeParams, init.info)

  private def buildTypeParameters(owner: ClassSymbol, specialization: Specialization)(using Context) =
    val tps = newTypeParams(
      owner,
      specialization.unspecializedTypeParams.map(_.typeSymbol.name.asTypeName),
      EmptyFlags,
      targets => targets.map(t => specialization.symbol.typeParams.find(_.name == t.name).get.info.bounds)
    )
    
    tps.foreach(owner.enter(_, EmptyScope))

    // Replace old type parameters that were copied from original trait with new ones
    // inside the parents of the new trait 
    val tpMap: Map[Type, Type] = specialization.unspecializedTypeParams.zip(tps.map(_.typeRef)).toMap
    val freshTypeVarMap = new TypeMap:
      def apply(t: Type) = tpMap.applyOrElse(t, mapOver)

    def mapSelfType(st: Type | Symbol): Type | Symbol = st match
      case sym: Symbol => sym.copy(info = freshTypeVarMap(sym.info))
      case tpe: Type   => freshTypeVarMap(tpe)

    owner.info = ClassInfo(
      owner.owner.thisType, 
      owner, 
      owner.info.parents.map(freshTypeVarMap(_)), 
      owner.info.decls, 
      mapSelfType(owner.classInfo.selfInfo)
    ) 
  
  // Order is depended on in Erasure::typedClassDef and TypeErasure:eraseParent
  private def getImplementationClassParents(specialization: Specialization, owner: Option[ClassSymbol])(using Context) = 
    // Set using old unspecializedTypeParams and replace after.
    val specializedTrait = owner.map(_.typeRef.appliedTo(specialization.unspecializedTypeParams)) 
    
    val originalTraitSpecializedParent = AppliedType(
      specialization.symbol.typeRef, 
      specialization.mapTypeArguments(tpe => specializationOf(tpe), specialization.unspecializedTypeParams)
    )
    
    (defn.ObjectType, specializedTrait, originalTraitSpecializedParent)

  private def newImplementationClass(specialization: Specialization, owner: Option[ClassSymbol])(using Context) =
    val (objectParent, traitSpParent, originalTraitSpecializedParent) = getImplementationClassParents(specialization, owner)
    
    val parents = 
      if traitSpParent.nonEmpty then 
        List(objectParent, traitSpParent.get, originalTraitSpecializedParent) 
      else 
        List(objectParent, originalTraitSpecializedParent)

    val newImplementationClassSymbol = newNormalizedClassSymbol(
      specialization.symbol.owner.enclosingPackageClass,
      specialization.newImplementationClassName,
      Flags.Synthetic,
      parents,
      NoType,
      specialization.symbol.privateWithin,
      spanCoord(specialization.span),
      specialization.symbol.compilationUnitInfo
    )

    specialization.symbol
      .addAnnotation(Annotation.Child(newImplementationClassSymbol, newImplementationClassSymbol.span.startPos))

    buildTypeParameters(newImplementationClassSymbol, specialization)

    newImplementationClassSymbol.entered

  // intefaceSymbol: None if no interface; this only happens with the fully non-specialized $impl$ (raw) case 
  private def buildImplementationClassTree(specialization: Specialization, interface: Option[ClassSymbol], cls: ClassSymbol)(using Context) = {
    val (objectParent, traitSpParent_, originalTraitSpecializedParent_) = getImplementationClassParents(specialization, interface)

    /* Apply Type Param Fix */
    val tpMap: Map[Type, Type] = specialization.unspecializedTypeParams.zip(cls.typeParams.map(_.typeRef)).toMap
    val freshTypeVarMap = new TypeMap:
      def apply(t: Type) = tpMap.applyOrElse(t, mapOver)
    val traitSpParent = traitSpParent_.map(tp => freshTypeVarMap(tp))
    val originalTraitSpecializedParent = freshTypeVarMap(originalTraitSpecializedParent_)

    val init = newConstructor(cls, EmptyFlags, Nil, Nil, coord=spanCoord(specialization.span))
    val typeMap = new TypeMap {
      override def apply(t: Type) = specialization.specializedConstructorParamToArgumentTypeMap.applyOrElse(t, mapOver)
    }
    
    /* Create constructor and setup constructor type */
    val nonTypeParams = specialization.symbol.primaryConstructor.rawParamss.tail
    
    val oldTypeParams = specialization.unspecializedConstructorParams
    val initTypeParams = cls.typeParams.map {
      sym => sym.copy(owner = init, flags = (sym.flags &~ (Flags.Private | Flags.Deferred)))
    }
    
    // We need to map the parameter names to avoid a name clash with val params from parents 
    // (see tests/pos/specialized-trait-val-parameter.scala)
    val valueParams = nonTypeParams map {
      symbols => symbols map {
        param => param.copy(
          owner = init, 
          info = typeMap(param.info).substSym(oldTypeParams, initTypeParams), 
          name=param.name.expandedName(cls)
        )
      }
    } 

    initTypeParams.foreach(_.entered)      
    init.setParamss(initTypeParams :: valueParams)
    
    // Type Arg if specialized; otherwise we want our type param.
    init.info = specialization.symbol.primaryConstructor.info.appliedTo( 
      specialization.constructorTypeParams map {
        param => specialization.specializedConstructorParamToArgumentTypeMap.applyOrElse(
          param, _.subst(oldTypeParams, cls.typeParams.map(_.typeRef))
        )
      }
    )
    fixConstructor(init, cls, valueParams.map(_.map(_.name.asTermName)))

    /* Build param accessors */
    val paramAccessorss = valueParams.map {
      params => params map {
        sym => sym.copy(
          owner = cls, 
          flags = (sym.flags|Flags.LocalParamAccessor) &~ Flags.Param, 
          info  = sym.info.subst(initTypeParams, cls.typeParams.map(_.typeRef))
        )
      }
    }
    paramAccessorss.foreach(_.foreach(cls.enter(_)))

    /* Build class def tree */
    val newParamss = paramAccessorss.nestedMap(ref(_))
    val newParams1 = if (newParamss.length == 1) then newParamss ++ List(List()) else newParamss
    
    /* Re-expand varargs parameters from Seq[T] to *T for passing into parent constructor */
    val newParams2 = newParams1.nestedMap( param =>
      if param.symbol.info.hasAnnotation(defn.RepeatedAnnot) then ctx.typer.seqToRepeated(param) else param 
    )

    val opTree = New(objectParent, objectParent.classSymbol.primaryConstructor.asTerm, Nil)
    val tspTree = traitSpParent.map(tsp => New(tsp, tsp.classSymbol.primaryConstructor.asTerm, Nil))
    val opSpTree = 
      New(originalTraitSpecializedParent.typeConstructor)
        .select(TermRef(originalTraitSpecializedParent.typeConstructor, specialization.symbol.primaryConstructor.asTerm))
        .appliedToTypes(originalTraitSpecializedParent.argTypes)
        .appliedToArgss(newParams2)

    ClassDefWithParents(
      cls,
      DefDef(init.asTerm.entered), 
      if tspTree.nonEmpty then List(opTree, tspTree.get, opSpTree) else List(opTree, opSpTree),
      paramAccessorss.flatMap(syms => syms.map(sym => tpd.ValDef(sym.asTerm)))
    ).withSpan(specialization.span)
  }

  // Returns (new stmts including original, new symbols including original)
  private def transformStatements(stats1: List[Tree], cache: SpecializationCache)(using Context): (List[Tree], SpecializationCache) = {

    val inlineSpecializedMethods = new TreeMapWithPreciseStatContexts {
      override def transform(tree: Tree)(using Context): Tree = tree match {
        case MethodSpecialization(methSpec)
          if methSpec.isSpecialized || methSpec.isFullySpecializedToTopClassesOrNothing => 
            val inlinedTree = Inlines.inlineCall(tree)(using specializedTraitCtx)
            super.transform(inlinedTree)
        case tree => super.transform(tree)
      }
    }
    
    val stats = inlineSpecializedMethods.transform(stats1)

    val specializations1 = collectReferencedSpecializations(stats, cache)
    val generatedTraitStats = specializations1.getNewInterfaceSymbols.toList.map(buildInterfaceTraitTree)
    val generatedClassStats = specializations1.getNewImplementationSymbols.toList.map(buildImplementationClassTree)

    val specializations2 = specializations1.installNewInterfaceSymbols.installNewImplementationSymbols
    
    val generatedTraitStats1 = generatedTraitStats.map {
      case tree: TypeDef =>
        assert(tree.symbol.isInlineTrait)
        val inlined = {
          given Context = specializedTraitCtx
          Inlines.inlineParentInlineTraits(Inlines.checkAndTransformInlineTrait(tree))
        }
        
        // Inlined body may contain references to inline traits that need to be inlined as well
        // See tests/neg/specialized-trait-inlining-causes-implementation-required-loop-bad-manual.scala
        // specializeInlineTraits is responsible for inlining into D because it's not $sp$ or $impl$
        // Because this code is synthetic we won't run the phase on this code as part of the usual
        // megaphase transform so we need to do it manually.
        val newInlineCtx = specializedTraitCtx.fresh.setInlineTraitState(
          specializedTraitCtx.inlineTraitState.copyInPhase(InlineTraitState.InlineContext.InlineTraits)
        )
        transformFollowing(inlined)(using newInlineCtx)
        
    } 

    val generatedClassStats1 = generatedClassStats.map {
      case tree: TypeDef =>
        val inlined = Inlines.inlineParentInlineTraits(tree)(using specializedTraitCtx)
        val newInlineCtx = ctx.fresh.setInlineTraitState(
          ctx.inlineTraitState.copyInPhase(InlineTraitState.InlineContext.InlineTraits)
        )
        transformFollowing(inlined)(using newInlineCtx)
    }

    val (generatedTraitStatsFinal, generatedClassStatsFinal, specializationsFinal) = 
      if (generatedTraitStats1.isEmpty && generatedClassStats1.isEmpty)
        (generatedTraitStats1, generatedClassStats1, specializations2)
      else 
        val (generatedTraitStats2, specializations3) = transformStatements(generatedTraitStats1, specializations2)
        val (generatedClassStats2, specializations4) = transformStatements(generatedClassStats1, specializations3)
        
        (generatedTraitStats2, generatedClassStats2, specializations4)

    // Since the only change we make to stats1 => stats is inlining we could arguably "undo" 
    // the inlining and then redo it at the "correct" point later -  so we don't actually modify 
    // the tree in that way here. Then we would only be generating new class and trait stats and 
    // we wouldn't need a transform statements method at all
    // (just a "generateInlineTraitsInterfaceAndImplementation" or something), 
    // but not sure if that's worth doing (it would be throwing away work).
    (generatedTraitStatsFinal ++ generatedClassStatsFinal ++ stats, specializationsFinal) 
  }

  private def specializedTraitCtx(using Context): Context = 
    ctx.fresh.setInlineTraitState(ctx.inlineTraitState.copyInPhase(InlineTraitState.InlineContext.SpecializedTraits))

  private var specializedTraitCache = SpecializationCache(
    genInterfaceSymbol = newInterfaceTrait, 
    genImplementationSymbol = newImplementationClass
  )

  // TODO: I reckon to get the best miniphase style processing we can do everything except 
  // outputting the final $impl$ / $sp$ classes in the normal miniphase methods.
  // i.e. instead of folding with transformStatements 
  // (which is odd now that we don't actually transform anything except creating the new specializations) 
  // we have all of the cases for collecting specialized references in ordinary transformMethods or prepare methods and 
  // then we just spit out the final classes in this transformUnit function. 
  // As long as we remember to call transformFollowing on the synthetic classes (which we do) then 
  // we should be composable in the way that we want to be.
  override def transformUnit(tree: Tree)(using Context): Tree = 
    if !ctx.compilationUnit.hasSpecializations then tree 
    else tree match {
      case pkg@PackageDef(pid, stats) =>
        val (stats1, specializedTraitCache2) = transformStatements(stats, specializedTraitCache)
        
        specializedTraitCache = specializedTraitCache2 
          
        val grouped = stats1.groupBy(tree => tree.symbol.enclosingPackageClass)
          
        // We need to copy the existing package so we don't lose any attachments 
        // e.g. attachments used to calculate Wunused
        cpy.PackageDef(pkg)(Ident(defn.EmptyPackageVal.namedType),
          grouped.getOrElse(defn.RootClass, List()) :::
          grouped.getOrElse(defn.EmptyPackageClass, List()) :::
          grouped
            .toList
            .filter((pk, stmts) => pk != defn.RootClass && pk != defn.EmptyPackageClass)
            .map((pkg, stmts) => tpd.PackageDef(Ident(pkg.sourceModule.namedType), stmts))
        ).withType(defn.EmptyPackageVal.namedType)
      case t => t
    }

  private def collectReferencedSpecializations(stats: List[Tree], specializations: SpecializationCache)(using Context): SpecializationCache =
    stats.foldLeft(specializations) {
      (specializations, tree) => {
        tree.deepFold(specializations) {
          (specializations, tree) => tree match
            case tdef@TypeDef(name, tmpl: Template) if tdef.symbol.isSpecializedTrait => 
              if !tdef.symbol.isStatic then
                // The approach we use for flattening makes this quite tricky: 
                // see e.g. tests/neg/specialized-trait-scoped-inside-object-deep-nesting.scala.
                // In theory can scan the tree to find where to put the generated traits instead, 
                // but this still doesn't work cross-CU, so for now we ban.
                report.error(
                  """
                    Specialized traits may not be defined inside classes or traits 
                    this would make them path-dependent which is not currently supported); 
                    They may be defined inside objects.
                  """, 
                  tdef.symbol.srcPos
                )
                
              tdef.symbol.typeParams.foreach {
                param =>
                  val isVariant = param.paramVariance.isOneOf(Flags.Contravariant | Flags.Covariant) && 
                    Specialization.classSpecializedTypeParams(tdef.symbol).exists(tpe => tpe.typeSymbol == param)
                  if isVariant then
                    report.warning(VarianceInSpecializedTraitsLimitation(), param.srcPos)
              }
              specializations
            case Typed(Apply(Select(New(anon),ctor),List()), tpt: TypeTree) if anon.symbol.isAnonymousClass =>
              (tpt.tpe, tpt.span) match {
                case Specialization(spec) if spec.isFullySpecializedToTopClassesOrNothing => 
                  // We never inline into anonymous class instances (avoids cycles in inline trait inlining), 
                  // so all anonymous class instances must have a non-anonymous class final representation as an $impl$ class.
                  specializations.addErasedImplementation(spec)
                                                                                                                                          
                case Specialization(spec) if spec.isSpecialized => 
                  specializations.addInterfaceAndImplementation(spec)

                case _ => specializations
              }
            case Specialization(specialization) =>
              if (specialization.hasSpecializedParams) {
                // Block Vec[?] and similar
                specialization.specializedTypeArgs filter {
                  case _: WildcardType => true
                  case _: RealTypeBounds => true
                  case tpe => 
                    false
                } foreach { tpe => 
                    report.error(
                      "Wildcard types may not be substituted for Specialized type parameters.", 
                      ctx.source.atSpan(specialization.span)
                    )
                }
              }
              if (specialization.isSpecialized) {            
                specializations.addInterface(specialization)
              } else {
                // Check foo[S: Specialized] <= Vec[S: Specialized]
                specialization.specializedTypeArgs flatMap {
                  // For each type we are using in a Specialized position
                  // Find all type params within that type that are not marked as Specialized so we can error
                  arg =>            
                    arg.widen.dealias.namedPartsWith {
                      part => 
                        def hasCorrectlyMarkedTypeParam: Boolean = 
                          val owner = 
                            if part.typeSymbol.owner.isClass then 
                              part.typeSymbol.owner.primaryConstructor 
                            else 
                              part.typeSymbol.owner
                          
                          owner.paramSymss.flatten.exists {
                            sym => sym.info match {
                              case SpecializedEvidence(tpeArg) =>
                                tpeArg.typeSymbol.isTypeParam && tpeArg.typeSymbol.name == part.name
                              case _ => false
                            }
                          }
                      
                        part.typeSymbol.isTypeParam && !hasCorrectlyMarkedTypeParam
                    }
                } foreach { tpe => 
                  if tpe.denot.symbol.srcPos.span.exists then
                    report.error(
                      s"${tpe.typeSymbol} used in a Specialized position, so it must be marked as Specialized at its definition.", 
                      tpe.denot.symbol.srcPos
                    )
                }
                
                specializations
              }
            
            case app @ Apply(_, _) => tpd.methPart(app) match { // class / object Bar extends Foo[Int](params)
              case fun @ Select(New(tpt), init) if fun.symbol.isConstructor => tpd.allArgss(tree) match {
                  case typeArgs :: valueArgss => 
                    val spec = Specialization(fun.symbol.owner, typeArgs.map(_.tpe), app.span)
                    if spec.isSpecialized then specializations.addInterface(spec) else specializations
                  case _ => specializations
                }
              case _ => specializations
            }

            case _ => specializations
        }
      }
    }
end DesugarSpecializedTraits

object DesugarSpecializedTraits:
  val name: String = "desugarSpecializedTraits"
  val description: String = 
    """
    Identifies traits having type parameters that have the Specialized annotation and generates corresponding specialized versions
    """ // Replacement with specialized versions occurs in erasure.

  def isTopClass(s: Symbol)(using Context): Boolean =
    (s eq defn.AnyClass) || (s eq defn.AnyValClass) || (s eq defn.ObjectClass) || (s eq defn.AnyRefAlias)

  def isTopClassOrNothing(s: Symbol)(using Context): Boolean = (s eq defn.NothingClass) || isTopClass(s)

  def specializationOf(tp: Type)(using Context): Type =
    if (tp.isInstanceOf[ErrorType]) then 
      return defn.ObjectClass.typeRef 

    def isSimpleClassType(s: Symbol): Boolean =
      s.isClass && !s.is(Flags.Trait) && s.typeParams.isEmpty && s.isStatic
    
    def isSelfOrParentTopType(s: Symbol): Boolean = 
      isTopClassOrNothing(s) || isTopClassOrNothing(s.asClass.superClass)

    tp.baseClasses
      .find(c => isSimpleClassType(c) && isSelfOrParentTopType(c)) match 
        case None => ErrorType(em"Missing specialization type for $tp")
        case Some(tpe) => tpe.typeRef

  def isSameErasureBucket(tp1: Type, tp2: Type)(using Context): Boolean =
    val sp1 = specializationOf(tp1)
    val sp2 = specializationOf(tp2)
    (sp1 eq sp2) || isTopClassOrNothing(tp1.classSymbol) && isTopClassOrNothing(tp2.classSymbol)

end DesugarSpecializedTraits

/*
  Stores the specializations we have found in the program and the symbols for the interface traits and implementation classes
  that will replace them. We generate these symbols when we enter the specializations into the cache, via the functions
  we store in genInterfaceSymbol and genImplementationSymbol. 

  Model: Contains two levels:
           - interface/implementation symbols we have found since the last installNewInterface/ImplementationSymbols call
            (i.e. typically on this iteration) ("new")
           - Those we found prior to that call, that were thus installed by it or previously

  Invariant: (newImplementationSymbols ∪ implementationSymbols) ⊆ (interfaceSymbols ∪ newInterfaceSymbols) except 
  for the "erased" implementationsclass Foo$impl which correspond to the ordinary erased interface Foo which is not stored.
*/
object SpecializationCache:
  type SymbolMap = Map[Specialization, ClassSymbol]
  type GenInterfaceSymbol = (Specialization, SpecializationCache) => Context ?=> (ClassSymbol, SpecializationCache)
  type GenImplementationSymbol = (Specialization, Option[ClassSymbol]) => Context ?=> ClassSymbol

class SpecializationCache(
  private val newInterfaceSymbols: SpecializationCache.SymbolMap = Map.empty,
  private val newImplementationSymbols: SpecializationCache.SymbolMap = Map.empty,
  private val interfaceSymbols: SpecializationCache.SymbolMap = Map.empty,
  private val implementationSymbols: SpecializationCache.SymbolMap = Map.empty,
  private val genInterfaceSymbol: SpecializationCache.GenInterfaceSymbol,
  private val genImplementationSymbol: SpecializationCache.GenImplementationSymbol
):

  def copy(
    newInterfaceSymbols: SpecializationCache.SymbolMap = this.newInterfaceSymbols,
    newImplementationSymbols: SpecializationCache.SymbolMap = this.newImplementationSymbols,
    interfaceSymbols: SpecializationCache.SymbolMap = this.interfaceSymbols,
    implementationSymbols: SpecializationCache.SymbolMap = this.implementationSymbols,
    genInterfaceSymbol: SpecializationCache.GenInterfaceSymbol = this.genInterfaceSymbol,
    genImplementationSymbol: SpecializationCache.GenImplementationSymbol = this.genImplementationSymbol)
      = SpecializationCache(
        newInterfaceSymbols, 
        newImplementationSymbols, 
        interfaceSymbols, 
        implementationSymbols, 
        genInterfaceSymbol, 
        genImplementationSymbol
      )

  def getInterfaceSymbol(spec: Specialization): Option[ClassSymbol] = 
    newInterfaceSymbols.orElse(interfaceSymbols).lift(spec)
  
  def getImplementationSymbol(spec: Specialization): Option[ClassSymbol] = 
    newImplementationSymbols.orElse(implementationSymbols).lift(spec)

  def getNewInterfaceSymbols: List[(Specialization, ClassSymbol)] = newInterfaceSymbols.toList
  
  def getNewImplementationSymbols: List[(Specialization, Option[ClassSymbol], ClassSymbol)] = 
    newImplementationSymbols.map((k, v) => (k, getInterfaceSymbol(k), v)).toList

  def addInterface(spec: Specialization)(using Context): SpecializationCache = 
    if (newInterfaceSymbols.contains(spec) || interfaceSymbols.contains(spec)) then
      this
    else
      val (targetSymbol, resultingCache) = genInterfaceSymbol(spec, this)
      resultingCache.copy(newInterfaceSymbols = resultingCache.newInterfaceSymbols + (spec -> targetSymbol))
      
  def addErasedImplementation(spec: Specialization)(using Context): SpecializationCache =
    val erased = Specialization(
      spec.symbol, 
      spec.mapTypeArguments(_ => defn.AnyClass.typeRef, spec.unspecializedTypeArgs), 
      spec.span
    )

    if (newImplementationSymbols.contains(erased) || implementationSymbols.contains(erased)) then
      this
    else
      copy(
        newImplementationSymbols = 
          newImplementationSymbols + (erased -> genImplementationSymbol(erased, getInterfaceSymbol(erased)))
      )
      
  def addInterfaceAndImplementation(spec: Specialization)(using Context): SpecializationCache = 
    if (newImplementationSymbols.contains(spec) || implementationSymbols.contains(spec)) then
      this
    else
      val withInterface = addInterface(spec)
      withInterface.copy(
        newImplementationSymbols = 
          withInterface.newImplementationSymbols + (spec -> genImplementationSymbol(spec, withInterface.getInterfaceSymbol(spec)))
      )

  def installNewInterfaceSymbols =
    this.copy(
      newInterfaceSymbols = Map.empty,
      interfaceSymbols = interfaceSymbols ++ newInterfaceSymbols)

  def installNewImplementationSymbols =
    this.copy(
      newImplementationSymbols = Map.empty,
      implementationSymbols = implementationSymbols ++ newImplementationSymbols)

end SpecializationCache

/* Represents an application traitSymbol[typeArguments] */
class Specialization(val symbol: Symbol, val args: List[Type], val span: Span)(using Context):
  
  private val paramToArgList = symbol.typeParams.map(_.typeRef).zip(args)

  // Type parameters marked with Specialized
  val specializedTypeParams: List[Type] = Specialization.classSpecializedTypeParams(symbol)
  
  // Type parameters not marked with Specialized
  val unspecializedTypeParams: List[Type] = 
    paramToArgList
      .filterNot((tParam, tArg) => specializedTypeParams.exists(_ =:= tParam))
      .map(_._1) 
  
  // Type arguments provided to parameters that are marked with Specialized at their definition
  val specializedTypeArgs: List[Type] = 
    paramToArgList
      .filter((tParam, tArg) => specializedTypeParams.exists(_ =:= tParam))
      .map(_._2)

  // Type arguments provided to parameters that are not marked with Specialized at their definition 
  val unspecializedTypeArgs: List[Type] = 
    paramToArgList
      .filterNot((tParam, tArg) => specializedTypeParams.exists(_ =:= tParam))
      .map(_._2) 

  val specializedTypeParamsToTypeArgumentsMap: Map[Type, Type] = 
    paramToArgList
      .toMap
      .filter((k, v) => specializedTypeParams.exists(_ =:= k))
      .map { 
        case (param, arg) => param -> specializationOf(arg)
      }
  
  val specialization: List[Type] = 
    symbol.typeParams
      .map(_.typeRef)
      .map(param => specializedTypeParamsToTypeArgumentsMap.applyOrElse(param, _ => param)) 

  val hasSpecializedParams: Boolean = specializedTypeParams.nonEmpty
  
  def constructorTypeParams: List[Type] = symbol.primaryConstructor.rawParamss.head.map(_.typeRef)
  
  def unspecializedConstructorParams: List[Symbol] = 
    symbol.primaryConstructor.rawParamss
      .head
      .zip(symbol.typeParams)
      .filterNot((constrParam, typeParam) => specializedTypeParams.exists(_ =:= typeParam.typeRef))
      .map((constrParam, typeParam) => constrParam)
  
  def specializedConstructorParamToArgumentTypeMap: Map[Type, Type] = 
    symbol.primaryConstructor.rawParamss
      .head
      .map(_.typeRef)
      .zip(paramToArgList)
      .filter((constrParam, paramArg) => specializedTypeParams.exists(_ =:= paramArg._1))
      .map((constrParam, paramArg) => (constrParam, specializationOf(paramArg._2)))
      .toMap

  def mapTypeArguments(specialize: Type => Type, nonspecialized: List[Type]): List[Type] = {
    val (result, _) = paramToArgList.foldLeft(Vector.empty[Type], nonspecialized) {
      case ((accumulator, defaults), (param, arg)) => 
        if specializedTypeParams.exists(_ =:= param) then 
          (accumulator :+ specialize(arg), defaults)
        else defaults match
          case head :: next => (accumulator :+ head, next)
          case Nil => (accumulator, Nil)
    }
    
    result.toList
  }

  /* If inline trait Foo[T: Specialized] has a method taking another Foo[T] there's no point specializing the reference
     since the resulting sp$T$ would be the same as the starting trait. Also A[Object] specializes to A. */
  def isSpecialized: Boolean = 
    hasSpecializedParams && specializedTypeArgs.forall(tpe => !isTopClassOrNothing(specializationOf(tpe).classSymbol))
  
  // Only works before erasure.
  def isFullySpecialized: Boolean =
    !specializedTypeArgs.exists(_.existsPart(part => (part.typeSymbol.isTypeParam)))
  
  def isFullySpecializedToTopClassesOrNothing: Boolean =
    hasSpecializedParams && 
      isFullySpecialized && 
      specializedTypeArgs.forall(tpe => isTopClassOrNothing(specializationOf(tpe).classSymbol))
    
  // TODO: Do we want to compress this more by adopting e.g. specializedTypeNames from scala 2? 
  // Using the exact Scala 2 naming scheme might be tricky since we currently check if "$$sp$" is contained within 
  // the name in order to figure out if it is a Scala 3 specialization specialized trait interface, 
  // therefore we need to avoid name conflicts. 
  // However we could keep our existing scheme but use Scala 2 specialization naming scheme for the typenames 
  // after the separator e.g. I instead of scala.Int. 
  // That is probably worth doing from a compression standpoint. 
  // Alternatively to avoid the conflict we can create our own  NameKind for scala 3 specialized names and 
  // then use the presence of that to determine if a symbol comes from scala 3 specialization.
  def canonicalName(tp: Type)(using Context): String = tp.dealias match
    case AppliedType(tycon, args) =>
      canonicalName(tycon) + args.map(canonicalName).mkString("$_$")
    case other =>
      other.typeSymbol.fullName.toString.replace('.', '$')
      
  def getTypeMap(using Context) = new TypeMap {
    override def apply(tp: Type): Type = specializedTypeParamsToTypeArgumentsMap.applyOrElse(tp, mapOver)
  }

  def generateName(suffix: String)(using Context) = 
    val className = (symbol.name ++ suffix ++ "$").asTypeName 
    val args = specializedTypeArgs
      .map(tpe => canonicalName(specializationOf(tpe)))
      .mkString(str.SPECIALIZED_TRAIT_TYPE_SEP)
    
    val name = className ++ args
    if symbol.owner.is(Flags.Package) then
      name
    else
      FlatName(symbol.owner.flatName.toTermName, name.toTermName).toSimpleName.toTypeName

  def newImplementationClassName(using Context): TypeName = 
    if isFullySpecializedToTopClassesOrNothing then
      (symbol.name ++ str.SPECIALIZED_TRAIT_IMPL_SUFFIX).asTypeName
    else
      generateName(str.SPECIALIZED_TRAIT_IMPL_SUFFIX)
      
  def newSpecializedTraitName(using Context): TypeName = 
    generateName(str.SPECIALIZED_TRAIT_SUFFIX)

  // We only care about the specialized arguments for equality; 
  // a specialization of Vec[A: Specialized, B] with B = Int and one with B = String 
  // can be considered to be the same as they use the same specialized trait
  override def equals(obj: Any): Boolean = obj match
    case obj: Specialization => 
      obj.symbol == symbol && 
      specializedTypeArgs.zip(obj.specializedTypeArgs).forall((a1, a2) => specializationOf(a1) == specializationOf(a2))
    case _ => false

  override def hashCode(): Int = 
    (symbol, specializedTypeArgs.map(tpe => specializationOf(tpe))).hashCode()
  
  override def toString(): String = 
    s"Specialization(${symbol}, ${args}, ${span})"
end Specialization

/* Represents an application methodSymbol[typeArguments](termArgs1)(termArgs2) etc */
class MethodSpecialization(val methodSymbol: Symbol, val typeArgss: List[List[Tree]])(using Context):
  val specializedTypeParams: List[Type] = 
    methodSymbol.paramSymss
      .flatten
      // Type parameters marked with Specialized
      .collect {
        denot => denot.info match { 
          case SpecializedEvidence(typeVar) => typeVar 
        } 
      } 
  private val paramToArgList = 
    methodSymbol.paramSymss
      .filter(l => l.nonEmpty && l.head.is(Flags.TypeParam))
      .zip(typeArgss)
      .map((params, args) => params.map(_.typeRef.asInstanceOf[Type]).zip(args))
      .flatten

  val specializedTypeArgs: List[Tree] = 
    paramToArgList
      .filter((tParam, tArg) => specializedTypeParams.exists(_ =:= tParam))
      .map(_._2) 

  val hasSpecializedParams: Boolean = specializedTypeParams.nonEmpty

  def isSpecialized: Boolean = 
    methodSymbol.isSpecializedMethod && 
    hasSpecializedParams && 
    specializedTypeArgs.exists(tree => !isTopClass(specializationOf(tree.tpe).classSymbol))
  
  def isFullySpecialized: Boolean =
    !specializedTypeArgs.exists(_.tpe.existsPart(part => (part.typeSymbol.isTypeParam)))
  
  def isFullySpecializedToTopClassesOrNothing: Boolean =
    methodSymbol.isSpecializedMethod && 
    hasSpecializedParams && 
    isFullySpecialized && 
    specializedTypeArgs.forall(tr => isTopClassOrNothing(specializationOf(tr.tpe).classSymbol))

end MethodSpecialization

object SpecializedEvidence {
  def unapply(tpe: Type)(using Context): Option[Type] = tpe match {
    case AppliedType(tycon, List(tpeArg)) if (tycon =:= ctx.definitions.SpecializedClass.typeRef && tpeArg.typeSymbol.isTypeParam) => Some(tpeArg)
    case _ => None
  }
}

object Specialization:

  def unapply(tpt: Tree)(using Context): Option[Specialization] = tpt match {
    case AppliedTypeTree(specializedTrait: Ident, concreteTypeTrees: List[Tree]) => 
      Some(Specialization(specializedTrait.denot.symbol, concreteTypeTrees.map(_.tpe), tpt.span))
    case t: TypeTree => Specialization.unapply(t.tpe, t.span)
    case _ => None
  }
  
  def unapply(typeSpan: (Type, Span))(using Context): Option[Specialization] = typeSpan match {
    case (AppliedType(tycon: Type, args: List[Type]), span) => Some(Specialization(tycon.typeSymbol, args, span))
    case _ => None
  }

  def unapply(tpe: Type)(using Context): Option[Specialization] = tpe match {
    case AppliedType(tycon: Type, args: List[Type]) => Some(Specialization(tycon.typeSymbol, args, NoSpan))
    case _ => None
  }

  def classSpecializedTypeParams(classSym: Symbol)(using Context): List[Type] = 
    if !classSym.isClass || classSym.is(Flags.JavaDefined) then
      List.empty
    else
      classSym.unforcedDecls.implicitDecls.collect(_.info match { case SpecializedEvidence(typeVar) => typeVar })

  def methodSpecializedTypeParams(methodSym: Symbol)(using Context): List[Type] = 
    methodSym.paramSymss
      .flatten
      .collect {
        denot => denot.info match { 
          case SpecializedEvidence(typeVar) => typeVar 
        }
      }
   
  def anonymousClassIsSpecialized(tree: Tree)(using Context) = 
    tree match {
      case TypeDef(anon, Template(_, parentCalls: List[Tree], _, _)) =>
        parentCalls match {
          case _ :+ Apply(Apply(t, ctorArgs), ev) => // extends Object, parents of spec trait, spec trait
            val spec = Specialization.unapply(t.tpe.resultType.resultType, t.span)
            spec.get.hasSpecializedParams
          case _ => false
        }
      case _ => false
    } 

  def isSpecializedTrait(sym: Symbol)(using Context) = 
    sym.isClass && 
    sym.isAllOf(InlineTrait) && 
    classSpecializedTypeParams(sym).nonEmpty
  
  def isSpecializedMethod(sym: Symbol)(using Context) = 
    !sym.is(Flags.JavaDefined) && 
    sym.isAllOf(InlineMethod) && 
    methodSpecializedTypeParams(sym).nonEmpty
  
  def traitParamIsSpecialized(traitSym: Symbol, tParam: Symbol)(using Context) = 
    classSpecializedTypeParams(traitSym).exists(tp => tp.typeSymbol eq tParam)

end Specialization

object MethodSpecialization:
  
  def unapply(tree: Tree)(using Context) = tree match {
    case app: Apply => 
      val methSym = tpd.methPart(app).symbol
      if methSym.is(Flags.Method) then
        Some(MethodSpecialization(methSym, tpd.typeArgss(app)))
      else
        None  
    case _ => None
  } 

end MethodSpecialization

class AnonymousClassInstance(
  val srcPos: SrcPos,
  val symbol: Symbol,
  val body: List[Tree],
  val parentCalls: List[Tree],
  val ctor: Name,
  val typeTree: TypeTree
)

object AnonymousClassInstance:

  def unapply(tree: Tree)(using Context) = tree match {
    case Block(
          List(an@TypeDef(anon, tmpl@Template(_, parentCalls: List[Tree], _, _))),  
          Typed(Apply(Select(New(anon1),ctor), _), t: TypeTree)
        ) if anon1.symbol.isAnonymousClass && (anon1.symbol eq an.symbol) => 
      Some(AnonymousClassInstance(an.srcPos, an.symbol, tmpl.body, parentCalls, ctor, t)) 

    // Coverage testing creates this extra case
    case Block(
          List(an@TypeDef(anon, tmpl@Template(_, parentCalls: List[Tree], _, _))),  
          Typed(Block(bindings, Apply(Select(New(anon1),ctor), _)), t: TypeTree)
        ) if anon1.symbol.isAnonymousClass && (anon1.symbol eq an.symbol) => 
      Some(AnonymousClassInstance(an.srcPos, an.symbol, tmpl.body, parentCalls, ctor, t)) 
      
    case _ => None
  }

end AnonymousClassInstance 
