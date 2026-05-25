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

class DesugarSpecializedTraits extends MacroTransform, IdentityDenotTransformer:

  override def phaseName: String = DesugarSpecializedTraits.name
  override def description: String = DesugarSpecializedTraits.description
  override def changesMembers: Boolean = false
  override def changesParents: Boolean = true 
  override def allowsImplicitSearch: Boolean = true

  private def newInterfaceTrait(specialization: Specialization, specializations: SpecializedTraitCache)(using Context): (ClassSymbol, SpecializedTraitCache) = {
    val tm = new TypeMap: // TODO: Can we get this into the specialization ideally.
      def apply(t: Type) = specialization.specializedTypeParamsToTypeArgumentsMap.view.mapValues(_.tpe).applyOrElse(t, mapOver) // TODO: If we can do just types we can get rid of this 
  
    val inheritedParents = specialization.traitSymbol.denot.info.parents.filterNot(_.typeSymbol == defn.ObjectClass).map(tm(_))
    // Parents may be specializable and so we need to specialize them as well
    // See ArrayIterator extends Iterator in specialized-trait-collections-example.scala
    val specializations1 = inheritedParents.foldLeft(specializations)((specializations, parent) => 
        (parent, specialization.span) match {
          case Specialization(spec) if spec.isSpecialized => specializations.addInterface(spec) 
          case _ => specializations
        }
    )

    // Create new trait
    val parents = defn.ObjectType
                  :: AppliedTypeTree(Ident(specialization.traitSymbol.typeRef), specialization.specialization).tpe // original trait, specialized to Foo[Int]
                  :: inheritedParents                                                                              // parents of the original trait in the form Foo[Int] (later specialized to Foo$sp$Int)

    val traitSymbol = newNormalizedClassSymbol(
      specialization.traitSymbol.owner,
      DesugarSpecializedTraits.newSpecializedTraitName(specialization),
      Flags.Synthetic | Flags.Trait | Flags.Inline,
      parents,
      NoType, // TODO: What happens if the creator of the specialized inline trait provides a self type? 
      specialization.traitSymbol.privateWithin,
      spanCoord(specialization.span),
      specialization.traitSymbol.compilationUnitInfo
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
  private def fixConstructor(init: Symbol, traitOrClassSymbol: ClassSymbol, targetParamNames: List[List[TermName]] = List())(using Context) = 
    val rt = traitOrClassSymbol.typeRef.appliedTo(traitOrClassSymbol.typeParams.map(_.typeRef))
    def resultType(tpe: Type, targetParamNames: List[List[TermName]]): Option[Type] = 
      tpe match {
        case mt @ MethodType(paramNames) => targetParamNames match {
          case head :: tail => Some(mt.derivedLambdaType(head, mt.paramInfos, resultType(mt.resultType, tail).getOrElse(rt)))
          case Nil          => Some(mt.derivedLambdaType(paramNames, mt.paramInfos, resultType(mt.resultType, targetParamNames).getOrElse(rt)))
        }
        case pt : PolyType => Some(pt.derivedLambdaType(pt.paramNames, pt.paramInfos, resultType(pt.resType, targetParamNames).get))
        case _ => None
    }
    init.info = resultType(init.info, targetParamNames).get
    init.info = PolyType.fromParams(init.owner.typeParams, init.info)

  private def buildTypeParameters(traitOrClassSymbol: ClassSymbol, specialization: Specialization)(using Context) =
    val tps = newTypeParams(traitOrClassSymbol,
                  specialization.unspecializedTypeParams.map(_.typeSymbol.name.asTypeName),
                  EmptyFlags,
                  targets => targets.map(t => specialization.traitSymbol.typeParams.find(_.name == t.name).get.info.bounds)
              )
    tps.foreach(traitOrClassSymbol.enter(_, EmptyScope))

    // Replace old type parameters that were copied from original trait with new ones
    // inside the parents of the new trait 
    val tpMap: Map[Type, Type] = specialization.unspecializedTypeParams.zip(tps.map(_.typeRef)).toMap
    val freshTypeVarMap = new TypeMap:
      def apply(t: Type) = tpMap.applyOrElse(t, mapOver)

    // TODO: What happens if the creator of the specialized inline trait provides a self type?
    traitOrClassSymbol.info = ClassInfo(traitOrClassSymbol.owner.thisType, traitOrClassSymbol, traitOrClassSymbol.info.parents.map(freshTypeVarMap(_)), traitOrClassSymbol.info.decls) 

  private def generateImplementationClassParents(specialization: Specialization, interfaceSymbol: ClassSymbol)(using Context) = 
    val objectParent = defn.ObjectType
    val traitSpParent = interfaceSymbol.typeRef.appliedTo(specialization.unspecializedTypeParams) // Set using old unspecializedTypeParams and replace after.
    val originalTraitSpecializedParent = AppliedTypeTree(Ident(specialization.traitSymbol.typeRef), specialization.mapUnspecializedArgs(specialization.unspecializedTypeParams.map(TypeTree(_)))).tpe
    (objectParent, traitSpParent, originalTraitSpecializedParent)

  private def newImplementationClass(specialization: Specialization, interfaceSymbol: ClassSymbol)(using Context) =
    val (objectParent, traitSpParent, originalTraitSpecializedParent) = generateImplementationClassParents(specialization, interfaceSymbol)
    val parents = List(objectParent, traitSpParent, originalTraitSpecializedParent)

    val newImplementationClassSymbol = newNormalizedClassSymbol(
      specialization.traitSymbol.owner,
      DesugarSpecializedTraits.newImplementationClassName(specialization),
      Flags.Synthetic,
      parents,
      NoType, // TODO: What happens if the creator of the specialized inline trait provides a self type? 
      specialization.traitSymbol.privateWithin,
      spanCoord(specialization.span),
      specialization.traitSymbol.compilationUnitInfo
    )

    buildTypeParameters(newImplementationClassSymbol, specialization)

    newImplementationClassSymbol.entered

  // TODO: Do we want to share some code with the newSpecializedInterfaceTrait and buildInterfaceTraitTree?
  // TODO: Tidy this up a bit with functions
  private def buildImplementationClassTree(specialization: Specialization, interfaceSymbol: ClassSymbol, classSymbol: ClassSymbol)(using Context) = {
    val (objectParent, traitSpParent_, originalTraitSpecializedParent_) = generateImplementationClassParents(specialization, interfaceSymbol)

    // Apply Type Param Fix: TODO : This really ought to be done more cleanly somewhere else.
    val tpMap: Map[Type, Type] = specialization.unspecializedTypeParams.zip(classSymbol.typeParams.map(_.typeRef)).toMap
    val freshTypeVarMap = new TypeMap:
      def apply(t: Type) = tpMap.applyOrElse(t, mapOver)
    val traitSpParent = freshTypeVarMap(traitSpParent_)
    val originalTraitSpecializedParent = freshTypeVarMap(originalTraitSpecializedParent_)

    val init = newConstructor(classSymbol, EmptyFlags, Nil, Nil, coord=spanCoord(specialization.span))
    val tm = new TypeMap: // TODO: Can we get this into the specialization ideally.
      def apply(t: Type) = specialization.specializedConstructorParamToArgumentTypeMap.applyOrElse(t, mapOver)
    
    /* Create constructor and setup constructor type */
    val nonTypeParams = specialization.traitSymbol.primaryConstructor.rawParamss.tail
    val oldTypeParams = specialization.unspecializedConstructorParams
    val initTypeParams = classSymbol.typeParams.map(s => s.copy(owner = init, flags = (s.flags &~ (Flags.Private | Flags.Deferred))))
    val valueParams = nonTypeParams.map(_.map(param => param.copy(owner = init, info = tm(param.info).substSym(oldTypeParams, initTypeParams), name=param.name.expandedName(classSymbol)))) // We need to map the parameter names to avoid a name clash with val params from parents (see tests/pos/specialized-trait-val-parameter.scala)

    initTypeParams.foreach(_.entered)      
    init.setParamss(initTypeParams :: valueParams)
    init.info = specialization.traitSymbol.primaryConstructor.info.appliedTo( // Type Arg if specialized; otherwise we want our type param.
      specialization.constructorTypeParams.map(par => specialization.specializedConstructorParamToArgumentTypeMap.applyOrElse(par, _.subst(oldTypeParams, classSymbol.typeParams.map(_.typeRef))))
    )
    fixConstructor(init, classSymbol, valueParams.map(_.map(_.name.asTermName)))

    /* Build param accessors */
    val paramAccessorss = valueParams.map(params => params.map(s => s.copy(owner = classSymbol, flags=(s.flags|Flags.LocalParamAccessor) &~ Flags.Param, info = s.info.subst(initTypeParams, classSymbol.typeParams.map(_.typeRef)))))
    paramAccessorss.foreach(_.foreach(classSymbol.enter(_)))

    /* Build class def tree */
    val newParamss = paramAccessorss.nestedMap(ref(_))
    val newParams1 = if (newParamss.length == 1) then newParamss ++ List(List()) else newParamss
    // TODO: Clean and robust
    ClassDefWithParents(
      classSymbol,
      DefDef(init.asTerm.entered), 
      List(
        New(objectParent, objectParent.classSymbol.primaryConstructor.asTerm, Nil),
        New(traitSpParent, traitSpParent.classSymbol.primaryConstructor.asTerm, Nil),
        New(originalTraitSpecializedParent.typeConstructor)
          .select(TermRef(originalTraitSpecializedParent.typeConstructor, specialization.traitSymbol.primaryConstructor.asTerm)) // TODO: Check for other constructors
          .appliedToTypes(originalTraitSpecializedParent.argTypes)
          .appliedToArgss(newParams1)
        ),
        // Put into body of class
        paramAccessorss.flatMap(syms => syms.map(sym => tpd.ValDef(sym.asTerm)))
      ).withSpan(specialization.span)
  }

  /* We can replace implementation class already because they implement both Foo[Int] and Foo$sp$Int so they are
     a drop in replacement for the anonymous classes that implement Foo[Int], and so we can swap them in without 
     modifying signatures / interfaces, which we have to do later at erasure. We don't really want to wait with 
     this replacement because it's easier to detect the anonymous classes earlier before they undergo too many transforms, 
     and doing it at erasure would be strange given it's a tree transform and not a type transform. */  
  private def replaceImplementationClassesMap(specializations: SpecializedTraitCache)(using Context) =
    val specializeTypeTree: Tree => Tree = tree => 
      tree match {
          case Specialization(spec) =>
            ctx.specializedTraitState.specializedTraitCache.get.getInterfaceSymbol(spec).map:
              specializedSymbol => 
              if spec.unspecializedTypeArgs.nonEmpty then
                AppliedTypeTree(Ident(specializedSymbol.typeRef), spec.unspecializedTypeArgs).withSpan(tree.span)  // TODO: Matching on a Specialization and then outputting ATT is weird - maybe have a method on specialization to convert to ATT .toAppliedTypeTree?
              else
                TypeTree(specializedSymbol.typeRef).withSpan(tree.span)
            .getOrElse(tree)
          case tree => tree
        }

    def treeMap(tree: Tree): Tree = tree match {
      /* Replace new Foo[Int] {} with new Foo$impl$Int.asInstanceOf[Foo$sp$Int]
          This has already been desugared to an anonymous class instance with parents:
          Objects, Parents of Foo, Foo. */

      case AnonymousSpecializationInstance(anon) =>
        def deandify(tp: Type): Iterator[Type] = tp match
          case AndType(l, r) => deandify(l) ++ deandify(r)
          case _ => Iterator.single(tp)
        anon.typeTree.tpe match {
          case a: AndType => /* Multiple mixed in traits will be typed as an AndType */ 
            deandify(a).foreach(trt =>
              Specialization.unapply(trt, anon.typeTree.span).foreach {spec => 
                if spec.hasSpecializedParams then
                  report.error("Anonymous classes acting as instances of Specialized traits may not mix in other traits; you can make a named object instead if you like.", anon.srcPos)
              }
            )
            tree
          case tpe =>
            Specialization.unapply(tpe, anon.typeTree.span).map(spec => 
                {
                if spec.hasSpecializedParams then
                  if anon.body.filterNot(x => x.symbol.name.is(ContextBoundParamName)).nonEmpty then // Only allowed to contain evidence parameters
                    report.error("Anonymous classes acting as instances of Specialized traits may not have additional members; you can make a named object instead if you like.", anon.srcPos)

                  anon.parentCalls match { 
                    case (obj :: parentsOfSpecTrait) :+ (app@Apply(_, _)) if (obj.symbol.owner == ctx.definitions.ObjectClass) && (parentsOfSpecTrait.forall(x => spec.traitSymbol.asClass.parentSyms.exists(p => p == x.symbol.owner))) =>
                      specializations.getImplementationSymbol(spec).map( specializedSymbol =>
                        inContext(ctx.withSource(anon.typeTree.source)) {
                          Typed(
                            Select(New(ref(specializedSymbol)), anon.ctor)
                              .appliedToTypeTrees(spec.unspecializedTypeArgs)
                              .appliedToArgss(tpd.allArgss(app).tail.nestedMap(_.changeNonLocalOwners(anon.symbol.owner))) // Skip the type params which are not needed
                          , specializeTypeTree(anon.typeTree))
                        }.withSpan(anon.typeTree.span)
                      ).getOrElse(tree) // We don't replace non-specialized anonymous class instantiations e.g. new Foo[T] where T is defined in the enclosing scope.
                    case _ => 
                      report.error("Anonymous classes acting as instances of Specialized traits may not mix in other traits; you can make a named object instead if you like.", anon.srcPos)
                      tree
                  }
                else
                  tree
              }).getOrElse(tree)
        }
      case tree => tree
    }
    new TreeTypeMap(treeMap = treeMap)
  end replaceImplementationClassesMap

  /* Override flags can be generated by inline trait inlining, but after removing the Foo[Int] parent the corresponding members no longer override members in their parents.
      Therefore we need to remove them. */
  def removeRedundantOverridesMap(using Context) = new TreeTypeMap(treeMap =
      tree => tree match {
        case dd@DefDef(name, paramss, tpt, preRhs) => 
          if dd.symbol.exists && dd.symbol.allOverriddenSymbols.isEmpty && (dd.symbol.owner.isSpecializedTraitInterface || dd.symbol.owner.isSpecializedTraitImplementationClass) then
            dd.symbol.flags = dd.symbol.flags &~ Flags.Override
          dd

        case vd@ValDef(name, tpt, preRhs) => 
          if vd.symbol.exists && vd.symbol.allOverriddenSymbols.isEmpty && (vd.symbol.owner.isSpecializedTraitInterface || vd.symbol.owner.isSpecializedTraitImplementationClass) then
            vd.symbol.flags = vd.symbol.flags &~ Flags.Override
          vd
        
        case tree => tree
    }
  )

  // Returns (new stmts including original, new symbols including original)
  private def transformStatements(stats1: List[Tree], specializations: SpecializedTraitCache)(using Context): (List[Tree], SpecializedTraitCache) = {

    val inlineSpecializedMethods = new TreeMapWithPreciseStatContexts {
      override def transform(tree: Tree)(using Context): Tree = tree match {
        case app: Apply if app.symbol.isSpecializedMethod =>
          val inlinedTree = Inlines.inlineCall(tree).asInstanceOf[Inlined]
          val callTrace = Inlines.inlineCallTrace(tree.symbol, inlinedTree.sourcePos)(using ctx.withSource(inlinedTree.source))
          val flattenedTree = cpy.Inlined(inlinedTree)(callTrace, inlinedTree.bindings, inlinedTree.expansion)(using inlineContext(inlinedTree))
          super.transform(flattenedTree)
        case tree => super.transform(tree)
      }
    }
    
    val stats = inlineSpecializedMethods.transform(stats1)

    val specializations1 = collectReferencedSpecializations(stats, specializations)
    val generatedTraitStats = specializations1.getNewInterfaceSymbols.toList.map(buildInterfaceTraitTree)
    val generatedClassStats = specializations1.getNewImplementationSymbols.toList.map(buildImplementationClassTree)

    val specializations2 = specializations1.installNewInterfaceSymbols.installNewImplementationSymbols

    /* We have Vec$sp$Int extends Vec[Int] in order to do the inlining, but then remove this parent 
      afterwards to avoid interface implementation problems (see tests/run/specialized-trait-as-parameter.scala,
      tests/run/specialized-trait-as-return-type.scala) */
    extension (classTree: Tree)
      def updateParents(parentUpdater: List[Type] => List[Type]) = (classTree: @unchecked) match {
        case td@TypeDef(name, t@Template(constr, preParentsOrDerived, self, preBody)) =>  

        val cls = td.symbol.asClass
        val oldInfo = cls.classInfo
        val newInfo = oldInfo.derivedClassInfo(declaredParents = parentUpdater(oldInfo.declaredParents))
        cls.info = newInfo
        cls.copySymDenotation(info = newInfo).installAfter(DesugarSpecializedTraits.this)
      }

      def refreshClassDef = (classTree: @unchecked) match {
        case td@TypeDef(name, t@Template(constr, preParentsOrDerived, self, preBody)) =>  
          ClassDef(td.symbol.asClass, constr, t.body)
      }

    /* We need to inline recursively throughout generated specialized traits - see tests/run/specialized-trait-requires-inline-trait-inlining.scala */
    // TODO: How do we calculate the spans correctly?
    val inlineInlineTraits = new TreeTypeMap(treeMap = (tree: Tree) => tree match {
      case tree: TypeDef if tree.symbol.isInlineTrait =>
        val tree1 = Inlines.checkAndTransformInlineTrait(tree)
        val tree2 = if Inlines.needsInlining(tree1) then Inlines.inlineParentInlineTraits(tree1) else tree1
        tree2
      case tree: TypeDef if Inlines.needsInlining(tree) =>
        Inlines.inlineParentInlineTraits(tree)
      case t => t
    })

    val generatedTraitStats1 = generatedTraitStats.map {
      case tree: TypeDef =>
        assert(tree.symbol.isInlineTrait)
        val inlined = Inlines.inlineParentInlineTraits(Inlines.checkAndTransformInlineTrait(tree),allowSpecialized=true).asInstanceOf[TypeDef]
        cpy.TypeDef(inlined)(name = inlined.name, rhs = inlineInlineTraits(inlined.rhs)).withSpan(inlined.span)
    } 

    val generatedClassStats1 = generatedClassStats.map {
      case tree: TypeDef =>
        assert(Inlines.needsInlining(tree, allowSpecializedTraits=true))
        val inlined = Inlines.inlineParentInlineTraits(tree, allowSpecialized=true).asInstanceOf[TypeDef]
        cpy.TypeDef(inlined)(name = inlined.name, rhs = inlineInlineTraits(inlined.rhs)).withSpan(inlined.span)
    }

    val (generatedTraitStatsFinal, generatedClassStatsFinal, specializationsFinal) = 
      if (generatedTraitStats1.isEmpty && generatedClassStats1.isEmpty)
        (generatedTraitStats1, generatedClassStats1, specializations2)
      else 
        val (generatedTraitStats2, specializations3) = transformStatements(generatedTraitStats1, specializations2)
        val (generatedClassStats2, specializations4) = transformStatements(generatedClassStats1, specializations3)
        
        /* We need to do the parent removal after inlining into the $impl$ classes otherwise we break
            overriding/interface implementation rules during the inlining. The $impl$ inlining
            can also happen in the recursive calls, and so we need to do this right at the end (after the recursive calls): */
        val generatedTraitStats3 = 
          generatedTraitStats2.tapEach: stat => 
            if stat.symbol.isSpecializedTraitInterface then // We could have $impl$ classes from recursive calls as well.
              stat.updateParents { parents => (parents: @unchecked) match
                case obj :: Specialization(originalSpec) :: parents if specializations4.getInterfaceSymbol(originalSpec).get == stat.symbol.asClass => 
                  obj :: parents
                case obj :: parents => obj :: parents // We already removed the relevant parent.
            }
          .map(stat => if stat.symbol.isSpecializedTraitInterface then refreshClassDef(stat) else stat)
        (generatedTraitStats3, generatedClassStats2, specializations4)

    val statsFinal = generatedTraitStatsFinal ++
                     generatedClassStatsFinal ++ 
                     stats.map(stat =>  replaceImplementationClassesMap(specializationsFinal)(
                       if (!stat.symbol.isSpecializedTraitImplementationClass && !stat.symbol.isSpecializedTraitInterface) then // We already processed these in an earlier recursive call
                         Inlines.inlineParentInlineTraits(stat, allowSpecialized = true, allowNonSpecialized = false) // Perform inlining into class Bar extends Foo[Int] from user code. // TODO: I don't really like this gating. 
                       else
                         stat
                     ))

    (statsFinal.map(removeRedundantOverridesMap(_)), specializationsFinal)
}
    
  override protected def newTransformer(using Context): Transformer = new Transformer:
    override def transform(tree: Tree)(using Context): Tree = 
      tree match { // TODO: Is Package level processing really what we want? Given we are going to output the classes somewhere else do we not really want either to deepFold the whole tree directly or do a more direct transform?
        case pkg@PackageDef(pid, stats) => // TODO: If we do everything ourselves and match only on the package then we can get rid of the MacroTransform aspect and just have a Phase with the transformPackageDef method.
          
          def checkType(t: Type, pos: SrcPos) = t.widen.dealias match {
            case Specialization.SpecializedEvidence(_) => 
              report.error(s"Only inline traits and inline functions may take Specialized type parameters", pos)
            case _ =>
          }

          tree.foreachSubTree { // TODO: This is not particularly efficient
            case d@DefDef(name, paramss, tpt, preRhs) if d.symbol.isConstructor && !d.symbol.owner.is(Flags.Inline) => d.paramss.flatten.foreach(p => checkType(p.tpe, d.srcPos))
            case d@DefDef(name, paramss, tpt, preRhs) if !d.symbol.isConstructor && !d.symbol.is(Flags.Inline) => d.paramss.flatten.foreach(p => checkType(p.tpe, d.srcPos))
            case _ =>
          }
          
          if ctx.specializedTraitState.specializedTraitCache.isEmpty then
             ctx.specializedTraitState.specializedTraitCache = Some(SpecializedTraitCache(genInterfaceSymbol = newInterfaceTrait, genImplementationSymbol = newImplementationClass))

          val (stats1, specializedTraitCache2) = transformStatements(stats, ctx.specializedTraitState.specializedTraitCache.get)
          ctx.specializedTraitState.specializedTraitCache = Some(specializedTraitCache2) // TODO: Avoid mutation here - we will make the cache mutable instead I think. Makes more sense
          cpy.PackageDef(pkg)(pid, stats1)
      }

  private def collectReferencedSpecializations(stats: List[Tree], specializations: SpecializedTraitCache)(using Context): SpecializedTraitCache =
    stats.foldLeft(specializations)((specializations, tree) => {
      tree.deepFold(specializations)((specializations, tree) => tree match
        case Typed(Apply(Select(New(anon),ctor),List()), t: TypeTree) if anon.symbol.isAnonymousClass =>
          (t.tpe, t.span) match {
            case Specialization(spec) if spec.isSpecialized => specializations.addInterfaceAndImplementation(spec)
            case _ => specializations
          }
        case Specialization(spec) =>
          if (spec.hasSpecializedParams) {
            // Block Vec[?] and similar
            spec.specializedTypeArgs.filter {
              case t: TypeBoundsTree => true
              case _ => false
            }.foreach: tr => 
                report.error("Wildcard types may not be substituted for Specialized type parameters.", tr.srcPos)
          }
          if (spec.isSpecialized) {            
            specializations.addInterface(spec)
          } else {
            // Check foo[S: Specialized] <= Vec[S: Specialized]
            spec.specializedTypeArgs.flatMap(arg => {           // For each type we are using in a Specialized position
              arg.tpe.widen.dealias.namedPartsWith(part =>      // Find all type params within that type that are not marked as Specialized so we can error
                part.typeSymbol.isTypeParam &&
                (!(if part.typeSymbol.owner.isClass then part.typeSymbol.owner.primaryConstructor else part.typeSymbol.owner).paramSymss.flatten.exists(
                  d => d.info match {
                    case Specialization.SpecializedEvidence(tpeArg) =>
                      tpeArg.typeSymbol.isTypeParam && tpeArg.typeSymbol.name == part.name
                    case _ => false
                  }
                ))
              )
            }).foreach: tr => 
              if tr.denot.symbol.srcPos.span.exists then
                report.error(s"${tr.typeSymbol} used in a Specialized position, so it must be marked as Specialized at its definition.", tr.denot.symbol.srcPos)
            specializations
          }
        
        case app @ Apply(_, _) => tpd.methPart(app) match { // class / object Bar extends Foo[Int](params)
          case fun @ Select(New(tpt), init) if fun.symbol.isConstructor => tpd.allArgss(tree) match {
              case typeArgs :: valueArgss => 
                val spec = Specialization(fun.symbol.owner, typeArgs, app.span)
                if spec.isSpecialized then specializations.addInterface(spec) else specializations
              case _ => specializations
            }
          case _ => specializations
        }

        case _ => specializations
      )
    })
end DesugarSpecializedTraits

object DesugarSpecializedTraits:
  val name: String = "desugarSpecializedTraits"
  val description: String = "Replaces traits having type parameters that have the Specialized annotation with specialized versions"

  // TODO: What happens with this name generation if we have Vec[Vec[T]] for example? We potentially don't have an Ident
  // TODO: Check what happens here when we have a case where the types being specialized into are user defined instead of primitives or type vars.
  private def generateName(specialization: Specialization, suffix: String)(using Context) = // TODO: Probably don't use show
    (specialization.traitSymbol.name ++ suffix).asTypeName ++ specialization.specializedTypeArgs.map(t => t.tpe.show).mkString(str.SPECIALIZED_TRAIT_TYPE_SEP)

  private[transform] def newSpecializedTraitName(specialization: Specialization)(using Context): TypeName = 
    generateName(specialization, str.SPECIALIZED_TRAIT_SUFFIX)

  private[transform] def newImplementationClassName(specialization: Specialization)(using Context): TypeName = 
    generateName(specialization, str.SPECIALIZED_TRAIT_IMPL_SUFFIX)
end DesugarSpecializedTraits

/*
  Stores the specializations we have found in the program and the symbols for the interface traits and implementation classes
  that will replace them. We generate these symbols when we enter the specializations into the cache, via the functions
  we store in genInterfaceSymbol and genImplementationSymbol. 

  Model: Contains two levels:
           - interface/implementation symbols we have found since the last installNewInterface/ImplementationSymbols call
            (i.e. typically on this iteration) ("new")
           - Those we found prior to that call, that were thus installed by it or previously

  Invariant: (newImplementationSymbols ∪ implementationSymbols) ⊆ (interfaceSymbols ∪ newInterfaceSymbols).
    This is enforced by only providing addInterface and addInterfaceAndImplementation, and allows the unchecked get in 
    getNewImplementationSymbols.

*/

object SpecializedTraitCache:
  type SymbolMap = Map[Specialization, ClassSymbol]
  type GenInterfaceSymbol = (Specialization, SpecializedTraitCache) => Context ?=> (ClassSymbol, SpecializedTraitCache)
  type GenImplementationSymbol = (Specialization, ClassSymbol) => Context ?=> ClassSymbol


class SpecializedTraitCache(
  private val newInterfaceSymbols: SpecializedTraitCache.SymbolMap = Map.empty,
  private val newImplementationSymbols: SpecializedTraitCache.SymbolMap = Map.empty,
  private val interfaceSymbols: SpecializedTraitCache.SymbolMap = Map.empty,
  private val implementationSymbols: SpecializedTraitCache.SymbolMap = Map.empty,
  private val genInterfaceSymbol: SpecializedTraitCache.GenInterfaceSymbol,
  private val genImplementationSymbol: SpecializedTraitCache.GenImplementationSymbol
):

  def copy(
    newInterfaceSymbols: SpecializedTraitCache.SymbolMap = this.newInterfaceSymbols,
    newImplementationSymbols: SpecializedTraitCache.SymbolMap = this.newImplementationSymbols,
    interfaceSymbols: SpecializedTraitCache.SymbolMap = this.interfaceSymbols,
    implementationSymbols: SpecializedTraitCache.SymbolMap = this.implementationSymbols,
    genInterfaceSymbol: SpecializedTraitCache.GenInterfaceSymbol = this.genInterfaceSymbol,
    genImplementationSymbol: SpecializedTraitCache.GenImplementationSymbol = this.genImplementationSymbol)
      = SpecializedTraitCache(newInterfaceSymbols, newImplementationSymbols, interfaceSymbols, implementationSymbols, genInterfaceSymbol, genImplementationSymbol)

  def getInterfaceSymbol(spec: Specialization): Option[ClassSymbol] = newInterfaceSymbols.orElse(interfaceSymbols).lift(spec)
  def getImplementationSymbol(spec: Specialization): Option[ClassSymbol] = newImplementationSymbols.orElse(implementationSymbols).lift(spec)

  def getNewInterfaceSymbols: List[(Specialization, ClassSymbol)] = newInterfaceSymbols.toList
  def getNewImplementationSymbols: List[(Specialization, ClassSymbol, ClassSymbol)] = newImplementationSymbols.map((k, v) => (k, getInterfaceSymbol(k).get, v)).toList

  def addInterface(spec: Specialization)(using Context): SpecializedTraitCache = 
    if (newInterfaceSymbols.contains(spec) || interfaceSymbols.contains(spec)) then
      this
    else
      val (targetSymbol, resultingCache) = genInterfaceSymbol(spec, this)
      resultingCache.copy(newInterfaceSymbols = resultingCache.newInterfaceSymbols + (spec -> targetSymbol))
  def addInterfaceAndImplementation(spec: Specialization)(using Context): SpecializedTraitCache = 
    if (newImplementationSymbols.contains(spec) || implementationSymbols.contains(spec)) then
      this
    else
      val withInterface = addInterface(spec)
      withInterface.copy(newImplementationSymbols = withInterface.newImplementationSymbols + (spec -> genImplementationSymbol(spec, withInterface.getInterfaceSymbol(spec).get)))

  def installNewInterfaceSymbols =
    this.copy(
      newInterfaceSymbols = Map.empty,
      interfaceSymbols = interfaceSymbols ++ newInterfaceSymbols)

  def installNewImplementationSymbols =
    this.copy(
      newImplementationSymbols = Map.empty,
      implementationSymbols = implementationSymbols ++ newImplementationSymbols)

end SpecializedTraitCache

/* Represents an application traitSymbol[typeArguments] */
class Specialization(val traitSymbol: Symbol, val typeArguments: List[Tree], val span: Span)(using Context): // TODO: Can we get away with List[Type]
  val specializedTypeParams: List[Type] = Specialization.classSpecializedTypeParams(traitSymbol) // Type parameters marked with Specialized
  
  // private val specializedTypeParamsSet = specializedTypeParams.toSet // TODO: We can bring this back if we manage to get the =:= type hashing but it's really not a big deal given the expected number of type parameters.
  private val paramToArgList = traitSymbol.typeParams.map(_.typeRef.asInstanceOf[Type]).zip(typeArguments)

  val unspecializedTypeParams: List[Type] = paramToArgList.filterNot((tParam, tArg) => specializedTypeParams.exists(_ =:= tParam)).map(_._1) // Type parameters not marked with Specialized
  val specializedTypeArgs: List[Tree] = paramToArgList.filter((tParam, tArg) => specializedTypeParams.exists(_ =:= tParam)).map(_._2) // Type arguments provided to parameters that are marked with Specialized at their definition
  val unspecializedTypeArgs: List[Tree] = paramToArgList.filterNot((tParam, tArg) => specializedTypeParams.exists(_ =:= tParam)).map(_._2) // Type arguments provided to parameters that are not marked with Specialized at their definition 

  val specializedTypeParamsToTypeArgumentsMap: Map[Type, Tree] = paramToArgList.toMap.filter((k, v) => specializedTypeParams.exists(_ =:= k))
  val specialization: List[Tree] = traitSymbol.typeParams.map(_.typeRef).map(specializedTypeParamsToTypeArgumentsMap.applyOrElse(_, TypeTree(_))) // TODO: Don't really like this name

  def constructorTypeParams: List[Type] = traitSymbol.primaryConstructor.rawParamss.head.map(_.typeRef)
  def unspecializedConstructorParams: List[Symbol] = traitSymbol.primaryConstructor.rawParamss.head.zip(traitSymbol.typeParams).filterNot((constrParam, typeParam) => specializedTypeParams.exists(_ =:= typeParam.typeRef)).map((constrParam, typeParam) => constrParam)
  def specializedConstructorParamToArgumentTypeMap: Map[Type, Type] = 
    traitSymbol.primaryConstructor.rawParamss.head.map(_.typeRef).zip(paramToArgList).filter((constrParam, paramArg) => specializedTypeParams.exists(_ =:= paramArg._1)).map((constrParam, paramArg) => (constrParam, paramArg._2.tpe)).toMap

  val hasSpecializedParams: Boolean = specializedTypeParams.nonEmpty

  def mapUnspecializedArgs(unspec: List[Tree]): List[Tree] = paramToArgList.foldLeft((List.empty[Tree], unspec))((resUnspec, paramArg) => ((resUnspec, paramArg): @unchecked) match {
    case ((result, unspec), (param, arg)) if specializedTypeParams.exists(_ =:= param) => (arg :: result, unspec)
    case ((result, head :: rest), (param, arg))                                        => (head :: result, rest)
  })._1.reverse

  /* If inline trait Foo[T] has a method taking another Foo[T] there's no point specializing the reference
     since the resulting sp$T$ would be the same as the starting trait. */
  def isSpecialized: Boolean = 
    hasSpecializedParams && typeArguments.exists(!_.tpe.existsPart(part => (part.typeSymbol.isTypeParam) ||
                                                                           (part.typeSymbol eq defn.AnyClass) || 
                                                                           (part.typeSymbol eq defn.ObjectClass) ||
                                                                           (part.typeSymbol eq defn.AnyValClass)))

  // Note: We only care about the specialized arguments for equality; a specialization of Vec[A: Specialized, B] with B = Int and one
  // with B = String can be considered to be the same as they use the same specialized trait
  // TODO: I don't really like this logic being in Specialization because they are really different
  // We should really put that logic in the SpecializedTraitCache because it's at that point that we treat them as the same.
  override def equals(obj: Any): Boolean = 
    obj.isInstanceOf[Specialization] && obj.asInstanceOf[Specialization].traitSymbol == traitSymbol
    && specializedTypeArgs.zip(obj.asInstanceOf[Specialization].specializedTypeArgs).forall((a1, a2) => a1.tpe =:= a2.tpe)

  override def hashCode(): Int = 
    (traitSymbol, specializedTypeArgs.map(_.tpe.widen.dealias.show)).hashCode() // TODO: Consider not using show for this for performance reasons (correctness also?)
  
  override def toString(): String = 
    s"Specialization(${traitSymbol}, ${typeArguments}, ${span})"
end Specialization

object Specialization:
  object SpecializedEvidence {
    def unapply(tpe: Type)(using Context): Option[Type] = tpe match {
      case AppliedType(tycon, List(tpeArg)) if (tycon =:= ctx.definitions.SpecializedClass.typeRef && tpeArg.typeSymbol.isTypeParam) => Some(tpeArg)
      case _ => None
    }
  }

  def unapply(tpt: Tree)(using Context): Option[Specialization] = tpt match {
    case AppliedTypeTree(specializedTrait: Ident, concreteTypeTrees: List[Tree]) => Some(Specialization(specializedTrait.denot.symbol, concreteTypeTrees, tpt.span))
    case t: TypeTree => Specialization.unapply(t.tpe, t.span)
    case _ => None
  }
  
  def unapply(typeSpan: (Type, Span))(using Context): Option[Specialization] = typeSpan match {
    case (AppliedType(tycon: Type, args: List[Type]), span) => Some(Specialization(tycon.typeSymbol, args.map(TypeTree(_)), span))
    case _ => None
  }

  def unapply(tpe: Type)(using Context): Option[Specialization] = tpe match {
    case AppliedType(tycon: Type, args: List[Type]) => Some(Specialization(tycon.typeSymbol, args.map(TypeTree(_)), NoSpan))
    case _ => None
  }

  def classSpecializedTypeParams(classSym: Symbol)(using Context): List[Type] = classSym.unforcedDecls.implicitDecls.collect(_.info match { case SpecializedEvidence(typeVar) => typeVar })
  def methodSpecializedTypeParams(methodSym: Symbol)(using Context): List[Type] = methodSym.paramSymss.flatten.collect(_.info match { case SpecializedEvidence(typeVar) => typeVar })
  
  // TODO: These methods are used in other phases; probably move them to the phase object? 
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

  def isSpecializedTrait(sym: Symbol)(using Context) = sym.isClass && sym.isAllOf(InlineTrait) && classSpecializedTypeParams(sym).nonEmpty
  def isSpecializedMethod(sym: Symbol)(using Context) = sym.isAllOf(InlineMethod) && methodSpecializedTypeParams(sym).nonEmpty 
end Specialization

class AnonymousSpecializationInstance(
  val srcPos: SrcPos,
  val symbol: Symbol,
  val body: List[Tree],
  val parentCalls: List[Tree],
  val ctor: Name,
  val typeTree: TypeTree
)

object AnonymousSpecializationInstance:
  def unapply(tree: Tree)(using Context) = tree match {
    case Block(List(an@TypeDef(anon, tmpl@Template(_, parentCalls: List[Tree], _, _))),  
              Typed(Apply(Select(New(anon1),ctor), _), t: TypeTree)) if anon1.symbol.isAnonymousClass => 
      Some(AnonymousSpecializationInstance(an.srcPos, an.symbol, tmpl.body, parentCalls, ctor, t)) 
    case _ => None
  }
end AnonymousSpecializationInstance 

class SpecializedTraitState:
  var specializedTraitCache: Option[SpecializedTraitCache] = None
end SpecializedTraitState


// Need to somehow make my naming a lot more consistent as well.
// figure out why we generate the T version.
// Try to see if we can do with only types and not trees
// Synthesise Specialized instances so that people can't do stupid stuff like Specialized[Array[T]]. type x = Specialized[Array[Array[Int]]]
// Set the Synthetic flags somewhere
// Cache / only generate once instead of multiple times.
// Ideally standardise on either specialization or specializationMap

// TODO: Think carefully about use of primaryConstructor and the other appropriateConstructors call or whatever it was.

// TODO: Need to try with a bigger project with multiple packages later on to see if we get the behaviour that we are expecting to get in terms of the classes that we generate.

// TODO: need to test with explicit evidence / our own custom type classes

// trait Vec$Sp[S] extends Vec[S, Int, Int, Int, Int]
// inline trait Two[S: Specialized] extends Vec$sp[S]
// Could potentially copy over the inline based on whether Two is inline or not? Needs some thought.

// In the case of foo[S](a: Vec[S, Int, Int, Int, Int]) I think we ideally do want this because we should be able to get speed gains by accessing the specialized members 

// TODO: Only specialize if there is some material increase in specialization - I think only if at least one new parameter gets fully specialized
// Maybe it is better to not allow partial specializations --  we can think about that.

// TODO: Don't synthesize specialized instances for random generic types probably - as Hamza said we want to be able to control the specialization

// Concerns:
//  - The superclass of `C` is a top class, or `C` itself is a top class.

// If we can manage to get rid of the inheritance there that could be helpful in terms of avoiding multiple values
// BUT: generate a version which is with just inline traits that has this problem as well.
// These implementation classes are type correct as long as we inject the knowledge that a specialization trait
// like `Seq$sp$Int` is equal to its parameterized version `Seq[Int]`
