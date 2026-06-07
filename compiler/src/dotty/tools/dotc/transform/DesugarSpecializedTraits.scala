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
import dotty.tools.dotc.transform.DesugarSpecializedTraits.specType
import dotty.tools.dotc.transform.DesugarSpecializedTraits.isTopClass
import dotty.tools.dotc.reporting.VarianceInSpecializedTraitsLimitation
import dotty.tools.dotc.transform.DesugarSpecializedTraits.isTopClassOrNothing

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

    // Order is depended on in Erasure::typedClassDef and TypeErasure:eraseParent
    val parents = defn.ObjectType
                  :: AppliedTypeTree(Ident(specialization.traitSymbol.typeRef), specialization.specialization).tpe // original trait, specialized to Foo[Int]
                  :: inheritedParents                                                                              // parents of the original trait in the form Foo[Int] (later specialized to Foo$sp$Int)

    // Create new trait
    val traitSymbol = newNormalizedClassSymbol(
      specialization.traitSymbol.owner.enclosingPackageClass, // For specialized traits defined inside objects/classes etc, pre-Flatten the $sp$ and $impl$ def trees (i.e.
                                                              // make them live in the enclosing package with the flattened name). We do this because it's easier than 
                                                              // finding the defining tree of the object, which would require scanning the whole file, and it
                                                              // might be in another compilation unit / already compiled.
      DesugarSpecializedTraits.newSpecializedTraitName(specialization),
      Flags.Synthetic | Flags.Trait | Flags.Inline,
      parents,
      tm(specialization.traitSymbol.asClass.classInfo.selfType),
      specialization.traitSymbol.privateWithin,
      spanCoord(specialization.span), // TODO: Show errors where they actually show up in the inline trait and not at the type of the user which is very confusing. Need to use inline stack
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

    def mapSelfType(st: Type | Symbol): Type | Symbol = 
      if st.isInstanceOf[Symbol] then
        st.asInstanceOf[Symbol].copy(info = freshTypeVarMap(st.asInstanceOf[Symbol].info))
      else
        freshTypeVarMap(st.asInstanceOf[Type])

    traitOrClassSymbol.info = ClassInfo(traitOrClassSymbol.owner.thisType, traitOrClassSymbol, traitOrClassSymbol.info.parents.map(freshTypeVarMap(_)), traitOrClassSymbol.info.decls, mapSelfType(traitOrClassSymbol.classInfo.selfInfo)) 
  
  // Order is depended on in Erasure::typedClassDef and TypeErasure:eraseParent
  private def generateImplementationClassParents(specialization: Specialization, interfaceSymbol: Option[ClassSymbol])(using Context) = 
    val objectParent = defn.ObjectType
    val traitSpParent = interfaceSymbol.map(_.typeRef.appliedTo(specialization.unspecializedTypeParams)) // Set using old unspecializedTypeParams and replace after.
    val originalTraitSpecializedParent = AppliedTypeTree(Ident(specialization.traitSymbol.typeRef), specialization.mapSpecializedUnspecializedArgs(tr => TypeTree(specType(tr.tpe)), specialization.unspecializedTypeParams.map(TypeTree(_)))).tpe
    (objectParent, traitSpParent, originalTraitSpecializedParent)

  private def newImplementationClass(specialization: Specialization, interfaceSymbol: Option[ClassSymbol])(using Context) =
    val (objectParent, traitSpParent, originalTraitSpecializedParent) = generateImplementationClassParents(specialization, interfaceSymbol)
    val parents = if traitSpParent.nonEmpty then List(objectParent, traitSpParent.get, originalTraitSpecializedParent) else List(objectParent, originalTraitSpecializedParent)

    val newImplementationClassSymbol = newNormalizedClassSymbol(
      specialization.traitSymbol.owner.enclosingPackageClass,
      DesugarSpecializedTraits.newImplementationClassName(specialization),
      Flags.Synthetic,
      parents,
      NoType,
      specialization.traitSymbol.privateWithin,
      spanCoord(specialization.span),
      specialization.traitSymbol.compilationUnitInfo
    )

    specialization.traitSymbol.addAnnotation(Annotation.Child(newImplementationClassSymbol, newImplementationClassSymbol.span.startPos))

    buildTypeParameters(newImplementationClassSymbol, specialization)

    newImplementationClassSymbol.entered

  // TODO: Do we want to share some code with the newSpecializedInterfaceTrait and buildInterfaceTraitTree?
  // TODO: Tidy this up a bit with functions
  // intefaceSymbol: None if no interface; this only happens with the fully non-specialized $impl$ (raw) case 
  private def buildImplementationClassTree(specialization: Specialization, interfaceSymbol: Option[ClassSymbol], classSymbol: ClassSymbol)(using Context) = {
    val (objectParent, traitSpParent_, originalTraitSpecializedParent_) = generateImplementationClassParents(specialization, interfaceSymbol)

    // Apply Type Param Fix: TODO : This really ought to be done more cleanly somewhere else.
    val tpMap: Map[Type, Type] = specialization.unspecializedTypeParams.zip(classSymbol.typeParams.map(_.typeRef)).toMap
    val freshTypeVarMap = new TypeMap:
      def apply(t: Type) = tpMap.applyOrElse(t, mapOver)
    val traitSpParent = traitSpParent_.map(tp => freshTypeVarMap(tp))
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
    
    // Re-expand varargs parameters from Seq[T] to *T for passing into parent constructor
    val newParams2 = newParams1.nestedMap( param =>
      if param.symbol.info.hasAnnotation(defn.RepeatedAnnot) then ctx.typer.seqToRepeated(param) else param 
    )

    val opTree = New(objectParent, objectParent.classSymbol.primaryConstructor.asTerm, Nil)
    val tspTree = traitSpParent.map(tsp => New(tsp, tsp.classSymbol.primaryConstructor.asTerm, Nil))
    val opSpTree = New(originalTraitSpecializedParent.typeConstructor)
          .select(TermRef(originalTraitSpecializedParent.typeConstructor, specialization.traitSymbol.primaryConstructor.asTerm))
          .appliedToTypes(originalTraitSpecializedParent.argTypes)
          .appliedToArgss(newParams2)

    // TODO: Clean and robust
    ClassDefWithParents(
      classSymbol,
      DefDef(init.asTerm.entered), 
      if tspTree.nonEmpty then List(opTree, tspTree.get, opSpTree)
      else List(opTree, opSpTree),
        // Put into body of class
      paramAccessorss.flatMap(syms => syms.map(sym => tpd.ValDef(sym.asTerm)))
    ).withSpan(specialization.span)
  }

  // Returns (new stmts including original, new symbols including original)
  private def transformStatements(stats1: List[Tree], specializations: SpecializedTraitCache)(using Context): (List[Tree], SpecializedTraitCache) = {

    val inlineSpecializedMethods = new TreeMapWithPreciseStatContexts {
      override def transform(tree: Tree)(using Context): Tree = tree match {
        case MethodSpecialization(methSpec) if methSpec.isSpecialized || methSpec.isFullySpecializedToTopClassesOrNothing =>
          // TODO: Not sure why we needed this - it doesn't seem to make any difference
          def flattenTree(inlinedTree: Tree): Tree = inlinedTree match {
            case it@Inlined(call, bindings, expansion) =>
              val callTrace = Inlines.inlineCallTrace(tree.symbol, inlinedTree.sourcePos)(using ctx.withSource(inlinedTree.source))
              cpy.Inlined(it)(callTrace, bindings, expansion)(using inlineContext(it))
            case Block(stats, expr) => Block(stats, flattenTree(expr)) 
            case t => t // if inlining failed due to max inlines reached
          }

          val inlinedTree = Inlines.inlineCall(tree)
          super.transform(flattenTree(inlinedTree))
        case tree => super.transform(tree)
      }
    }
    
    val stats = inlineSpecializedMethods.transform(stats1)

    val specializations1 = collectReferencedSpecializations(stats, specializations)
    val generatedTraitStats = specializations1.getNewInterfaceSymbols.toList.map(buildInterfaceTraitTree)
    val generatedClassStats = specializations1.getNewImplementationSymbols.toList.map(buildImplementationClassTree)

    val specializations2 = specializations1.installNewInterfaceSymbols.installNewImplementationSymbols
    
    val generatedTraitStats1 = generatedTraitStats.map {
      case tree: TypeDef =>
        assert(tree.symbol.isInlineTrait)
        val inlined = Inlines.inlineParentInlineTraits(Inlines.checkAndTransformInlineTrait(tree)).asInstanceOf[TypeDef]
        cpy.TypeDef(inlined)(name = inlined.name, rhs = inlined.rhs).withSpan(inlined.span)
    } 

    val generatedClassStats1 = generatedClassStats.map {
      case tree: TypeDef =>
        val inlined = Inlines.inlineParentInlineTraits(tree).asInstanceOf[TypeDef]
        cpy.TypeDef(inlined)(name = inlined.name, rhs = inlined.rhs).withSpan(inlined.span)
    }

    val (generatedTraitStatsFinal, generatedClassStatsFinal, specializationsFinal) = 
      if (generatedTraitStats1.isEmpty && generatedClassStats1.isEmpty)
        (generatedTraitStats1, generatedClassStats1, specializations2)
      else 
        val (generatedTraitStats2, specializations3) = transformStatements(generatedTraitStats1, specializations2)
        val (generatedClassStats2, specializations4) = transformStatements(generatedClassStats1, specializations3)
        
        (generatedTraitStats2, generatedClassStats2, specializations4)

    (generatedTraitStatsFinal ++ generatedClassStatsFinal ++ stats, specializationsFinal) // TODO: Since we only change stats by the inlining we could potentially arguably "undo" the inlining and then redo it at the "correct" point later - 
                                                                                          // so we don't actually modify the tree in that way here; not sure if that's worth doing (it would be throwing away work) 
  }
  
  private def checkSpecializedTraitRules(tree: Tree)(using Context) =
    def checkType(t: Type, pos: SrcPos) = t.widen.dealias match {
      case SpecializedEvidence(_) => 
        report.error(s"Only inline traits and inline functions may take Specialized type parameters", pos)
      case _ =>
    }
          
    tree.foreachSubTree { // TODO: This is not particularly efficient
      case d@DefDef(name, paramss, tpt, preRhs) if d.symbol.isConstructor && !d.symbol.owner.is(Flags.Inline) => d.paramss.flatten.foreach(p => checkType(p.tpe, d.srcPos))
      case d@DefDef(name, paramss, tpt, preRhs) if !d.symbol.isConstructor && !d.symbol.is(Flags.Inline) => d.paramss.flatten.foreach(p => checkType(p.tpe, d.srcPos))
      case AnonymousClassInstance(anon) =>
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
          case tpe =>
            Specialization.unapply(tpe, anon.typeTree.span).map(spec => 
                {
                if spec.hasSpecializedParams then
                  if anon.body.filterNot(x => x.symbol.name.is(ContextBoundParamName)).nonEmpty then // Only allowed to contain evidence parameters
                    report.error("Anonymous classes acting as instances of Specialized traits may not have additional members; you can make a named object instead if you like.", anon.srcPos)

                  anon.parentCalls match { 
                    case (obj :: parentsOfSpecTrait) :+ (app@Apply(_, _)) if (obj.symbol.owner == ctx.definitions.ObjectClass) && (parentsOfSpecTrait.forall(x => spec.traitSymbol.asClass.baseClasses.exists(p => p == x.symbol.owner))) => 
                    case _ => report.error("Anonymous classes acting as instances of Specialized traits may not mix in other traits; you can make a named object instead if you like.", anon.srcPos)
                  }
                else
                  tree
              }).getOrElse(tree)
        }

      case _ =>
    }

  override protected def newTransformer(using Context): Transformer = new Transformer:
    override def transform(tree: Tree)(using Context): Tree = 
      tree match { // TODO: Is Package level processing really what we want? Given we are going to output the classes somewhere else do we not really want either to deepFold the whole tree directly or do a more direct transform?
        case pkg@PackageDef(pid, stats) => // TODO: If we do everything ourselves and match only on the package then we can get rid of the MacroTransform aspect and just have a Phase with the transformPackageDef method or even transformStats ideally
          
          checkSpecializedTraitRules(tree)

          if ctx.specializedTraitState.specializedTraitCache.isEmpty then
             ctx.specializedTraitState.specializedTraitCache = Some(SpecializedTraitCache(genInterfaceSymbol = newInterfaceTrait, genImplementationSymbol = newImplementationClass))

          val (stats1, specializedTraitCache2) = transformStatements(stats, ctx.specializedTraitState.specializedTraitCache.get)
          ctx.specializedTraitState.specializedTraitCache = Some(specializedTraitCache2) // TODO: Avoid mutation here - we will make the cache mutable instead I think. Makes more sense
          
          val grouped = stats1.groupBy(tree => tree.symbol.enclosingPackageClass)
          
          cpy.PackageDef(pkg)(Ident(defn.EmptyPackageVal.namedType), // We need to copy the existing package so we don't lose any attachments (e.g. attachments used to calculate Wunused)
            grouped.getOrElse(defn.RootClass, List()) :::
            grouped.getOrElse(defn.EmptyPackageClass, List()) :::
            grouped.toList.filter((pk, stmts) => pk != defn.RootClass && pk != defn.EmptyPackageClass).map((pkg, stmts) => tpd.PackageDef(Ident(pkg.sourceModule.namedType), stmts))
          ).withType(defn.EmptyPackageVal.namedType)
        case t => t
      }

  private def collectReferencedSpecializations(stats: List[Tree], specializations: SpecializedTraitCache)(using Context): SpecializedTraitCache =
    stats.foldLeft(specializations)((specializations, tree) => {
      tree.deepFold(specializations)((specializations, tree) => tree match
        case t@TypeDef(name, tmpl: Template) if t.symbol.isSpecializedTrait => 
          if !t.symbol.isStatic then
            // The approach we use for flattening makes this quite tricky: see e.g. tests/neg/specialized-trait-scoped-inside-object-deep-nesting.scala.
            // In theory can scan the tree to find where to put the generated traits instead, but this still doesn't work cross-CU, so for now we ban.
            report.error("Specialized traits may not be defined inside classes or traits (this would make them path-dependent which is not currently supported); they may be defined inside objects.", t.symbol.srcPos)
          t.symbol.typeParams.foreach: par =>
            if par.paramVariance.isOneOf(Flags.Contravariant | Flags.Covariant) && Specialization.classSpecializedTypeParams(t.symbol).exists(t => t.typeSymbol == par) then
              report.warning(VarianceInSpecializedTraitsLimitation(), par.srcPos)
          specializations
        case Typed(Apply(Select(New(anon),ctor),List()), t: TypeTree) if anon.symbol.isAnonymousClass =>
          (t.tpe, t.span) match {
            case Specialization(spec) if spec.isFullySpecializedToTopClassesOrNothing => specializations.addErasedImplementation(spec) // We never inline into anonymous class instances (avoids cycles in inline trait inlining), so 
                                                                                                                                       // all anonymous class instances must have a non-anonymous class final representation as an $impl$ class.
            case Specialization(spec) if spec.isSpecialized                           => specializations.addInterfaceAndImplementation(spec)
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
                    case SpecializedEvidence(tpeArg) =>
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
  val description: String = "Identifies traits having type parameters that have the Specialized annotation and generates corresponding specialized versions" // Replacement with specialized versions occurs in erasure.

  // TODO: Do we want to compress this more by adopting e.g. specializedTypeNames from scala 2? 
  // TODO: NameKind?
  def canonicalName(tp: Type)(using Context): String = tp.dealias match
    case AppliedType(tycon, args) =>
      canonicalName(tycon) + args.map(canonicalName).mkString("$_$")
    case other =>
      other.typeSymbol.fullName.toString.replace('.', '$')


  // TODO: What happens with this name generation if we have Vec[Vec[T]] for example? We potentially don't have an Ident
  // TODO: Check what happens here when we have a case where the types being specialized into are user defined instead of primitives or type vars.
  private def generateName(specialization: Specialization, suffix: String)(using Context) = 
    val name = (specialization.traitSymbol.name ++ suffix ++ "$").asTypeName ++ specialization.specializedTypeArgs.map(t => canonicalName(specType(t.tpe))).mkString(str.SPECIALIZED_TRAIT_TYPE_SEP)
    if specialization.traitSymbol.owner.is(Flags.Package) then
      name
    else
      FlatName(specialization.traitSymbol.owner.flatName.toTermName, name.toTermName).toSimpleName.toTypeName

  /*private[transform]*/ def newSpecializedTraitName(specialization: Specialization)(using Context): TypeName = 
    generateName(specialization, str.SPECIALIZED_TRAIT_SUFFIX)

  /*private[transform]*/ def newImplementationClassName(specialization: Specialization)(using Context): TypeName = 
    if specialization.isFullySpecializedToTopClassesOrNothing then
      (specialization.traitSymbol.name ++ str.SPECIALIZED_TRAIT_IMPL_SUFFIX).asTypeName
    else
      generateName(specialization, str.SPECIALIZED_TRAIT_IMPL_SUFFIX)

  def isTopClass(s: Symbol)(using Context): Boolean =
    (s eq defn.AnyClass) || (s eq defn.AnyValClass) || (s eq defn.ObjectClass) || (s eq defn.AnyRefAlias)
  def isTopClassOrNothing(s: Symbol)(using Context): Boolean = (s eq defn.NothingClass) || isTopClass(s)

  def specType(tp: Type)(using Context): Type =

    def isSimpleClassType(s: Symbol): Boolean =
      s.isClass && !s.is(Flags.Trait) && s.typeParams.isEmpty && s.isStatic

    tp.baseClasses.iterator.find(c =>
      isSimpleClassType(c) && (isTopClassOrNothing(c) || isTopClassOrNothing(c.asClass.superClass))
    ).map(_.typeRef).get

  def isSameErasureBucket(tp1: Type, tp2: Type)(using Context) =
    val sp1 = specType(tp1)
    val sp2 = specType(tp2)
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

  Invariant: (newImplementationSymbols ∪ implementationSymbols) ⊆ (interfaceSymbols ∪ newInterfaceSymbols).
*/

object SpecializedTraitCache:
  type SymbolMap = Map[Specialization, ClassSymbol]
  type GenInterfaceSymbol = (Specialization, SpecializedTraitCache) => Context ?=> (ClassSymbol, SpecializedTraitCache)
  type GenImplementationSymbol = (Specialization, Option[ClassSymbol]) => Context ?=> ClassSymbol

// TODO: We don't need to share this between phases anymore and maybe we don't even need it at all. Can also rename. 
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
  def getNewImplementationSymbols: List[(Specialization, Option[ClassSymbol], ClassSymbol)] = newImplementationSymbols.map((k, v) => (k, getInterfaceSymbol(k), v)).toList

  def addInterface(spec: Specialization)(using Context): SpecializedTraitCache = 
    if (newInterfaceSymbols.contains(spec) || interfaceSymbols.contains(spec)) then
      this
    else
      val (targetSymbol, resultingCache) = genInterfaceSymbol(spec, this)
      resultingCache.copy(newInterfaceSymbols = resultingCache.newInterfaceSymbols + (spec -> targetSymbol))
  def addErasedImplementation(spec: Specialization)(using Context): SpecializedTraitCache =
    val erased = Specialization(spec.traitSymbol, spec.mapSpecializedUnspecializedArgs(_ => TypeTree(defn.AnyClass.typeRef), spec.unspecializedTypeArgs), spec.span)
    if (newImplementationSymbols.contains(erased) || implementationSymbols.contains(erased)) then
      this
    else
      this.copy(newImplementationSymbols = this.newImplementationSymbols + (erased -> genImplementationSymbol(erased, this.getInterfaceSymbol(erased))))
  def addInterfaceAndImplementation(spec: Specialization)(using Context): SpecializedTraitCache = 
    if (newImplementationSymbols.contains(spec) || implementationSymbols.contains(spec)) then
      this
    else
      val withInterface = addInterface(spec)
      withInterface.copy(newImplementationSymbols = withInterface.newImplementationSymbols + (spec -> genImplementationSymbol(spec, withInterface.getInterfaceSymbol(spec))))

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

  val specializedTypeParamsToTypeArgumentsMap: Map[Type, Tree] = paramToArgList.toMap.filter((k, v) => specializedTypeParams.exists(_ =:= k)).view.mapValues(tr => TypeTree(specType(tr.tpe))).toMap // TODO: Maybe not efficient
  val specialization: List[Tree] = traitSymbol.typeParams.map(_.typeRef).map(specializedTypeParamsToTypeArgumentsMap.applyOrElse(_, TypeTree(_))) // TODO: Don't really like this name

  def constructorTypeParams: List[Type] = traitSymbol.primaryConstructor.rawParamss.head.map(_.typeRef)
  def unspecializedConstructorParams: List[Symbol] = traitSymbol.primaryConstructor.rawParamss.head.zip(traitSymbol.typeParams).filterNot((constrParam, typeParam) => specializedTypeParams.exists(_ =:= typeParam.typeRef)).map((constrParam, typeParam) => constrParam)
  def specializedConstructorParamToArgumentTypeMap: Map[Type, Type] = 
    traitSymbol.primaryConstructor.rawParamss.head.map(_.typeRef).zip(paramToArgList).filter((constrParam, paramArg) => specializedTypeParams.exists(_ =:= paramArg._1)).map((constrParam, paramArg) => (constrParam, specType(paramArg._2.tpe))).toMap

  val hasSpecializedParams: Boolean = specializedTypeParams.nonEmpty

  def mapSpecializedUnspecializedArgs(spec: Tree => Tree, unspec: List[Tree]): List[Tree] = paramToArgList.foldLeft((List.empty[Tree], unspec))((resUnspec, paramArg) => ((resUnspec, paramArg): @unchecked) match {
    case ((result, unspec), (param, arg)) if specializedTypeParams.exists(_ =:= param) => (spec(arg) :: result, unspec)
    case ((result, head :: rest), (param, arg))                                        => (head :: result, rest)
  })._1.reverse

  /* If inline trait Foo[T: Specialized] has a method taking another Foo[T] there's no point specializing the reference
     since the resulting sp$T$ would be the same as the starting trait. Also A[Object] specializes to A. */
  def isSpecialized: Boolean = 
    hasSpecializedParams && specializedTypeArgs.exists(tree => !isTopClassOrNothing(specType(tree.tpe).classSymbol))
  // Only works before erasure.
  def isFullySpecialized: Boolean =
    !specializedTypeArgs.exists(_.tpe.existsPart(part => (part.typeSymbol.isTypeParam)))
  def isFullySpecializedToTopClassesOrNothing: Boolean =
    hasSpecializedParams && isFullySpecialized && specializedTypeArgs.forall(tr => isTopClassOrNothing(specType(tr.tpe).classSymbol))

  // Note: We only care about the specialized arguments for equality; a specialization of Vec[A: Specialized, B] with B = Int and one
  // with B = String can be considered to be the same as they use the same specialized trait
  // TODO: I don't really like this logic being in Specialization because they are really different
  // We should really put that logic in the SpecializedTraitCache because it's at that point that we treat them as the same.
  override def equals(obj: Any): Boolean = 
    obj.isInstanceOf[Specialization] && obj.asInstanceOf[Specialization].traitSymbol == traitSymbol
    && specializedTypeArgs.zip(obj.asInstanceOf[Specialization].specializedTypeArgs).forall((a1, a2) => specType(a1.tpe) == specType(a2.tpe))

  override def hashCode(): Int = 
    (traitSymbol, specializedTypeArgs.map(tr => specType(tr.tpe))).hashCode()
  
  override def toString(): String = 
    s"Specialization(${traitSymbol}, ${typeArguments}, ${span})"
end Specialization

/* Represents an application methodSymbol[typeArguments](termArgs1)(termArgs2) etc */
class MethodSpecialization(val methodSymbol: Symbol, val typeArgss: List[List[Tree]])(using Context):
  val specializedTypeParams: List[Type] = methodSymbol.paramSymss.flatten.collect(_.info match { case SpecializedEvidence(typeVar) => typeVar }) // Type parameters marked with Specialized
  private val paramToArgList = 
    methodSymbol.paramSymss.filter(l => l.nonEmpty && l.head.is(Flags.TypeParam)).zip(typeArgss).map(
    (params, args) => params.map(_.typeRef.asInstanceOf[Type]).zip(args) 
  ).flatten

  // TODO: Can we share these? General Specialization + Method + Trait
  val specializedTypeArgs: List[Tree] = paramToArgList.filter((tParam, tArg) => specializedTypeParams.exists(_ =:= tParam)).map(_._2) // Type arguments provided to parameters that are marked with Specialized at their definition

  val hasSpecializedParams: Boolean = specializedTypeParams.nonEmpty

  def isSpecialized: Boolean = 
    methodSymbol.isSpecializedMethod && hasSpecializedParams && specializedTypeArgs.exists(tree => !isTopClass(specType(tree.tpe).classSymbol))
  def isFullySpecialized: Boolean =
    !specializedTypeArgs.exists(_.tpe.existsPart(part => (part.typeSymbol.isTypeParam)))
  def isFullySpecializedToTopClassesOrNothing: Boolean =
    methodSymbol.isSpecializedMethod && hasSpecializedParams && isFullySpecialized && specializedTypeArgs.forall(tr => isTopClassOrNothing(specType(tr.tpe).classSymbol))
end MethodSpecialization

// TODO: If we can get this in Specialization when we do inheritance that would be great.
object SpecializedEvidence {
  def unapply(tpe: Type)(using Context): Option[Type] = tpe match {
    case AppliedType(tycon, List(tpeArg)) if (tycon =:= ctx.definitions.SpecializedClass.typeRef && tpeArg.typeSymbol.isTypeParam) => Some(tpeArg)
    case _ => None
  }
}

object Specialization:

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
  def traitParamIsSpecialized(traitSym: Symbol, tParam: Symbol)(using Context) = classSpecializedTypeParams(traitSym).exists(tp => tp.typeSymbol eq tParam)
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
    case Block(List(an@TypeDef(anon, tmpl@Template(_, parentCalls: List[Tree], _, _))),  
              Typed(Apply(Select(New(anon1),ctor), _), t: TypeTree)) if anon1.symbol.isAnonymousClass && (anon1.symbol eq an.symbol) => 
      Some(AnonymousClassInstance(an.srcPos, an.symbol, tmpl.body, parentCalls, ctor, t)) 

    // Coverage testing creates this extra case
    case Block(List(an@TypeDef(anon, tmpl@Template(_, parentCalls: List[Tree], _, _))),  
              Typed(Block(bindings, Apply(Select(New(anon1),ctor), _)), t: TypeTree)) if anon1.symbol.isAnonymousClass && (anon1.symbol eq an.symbol) => 
      Some(AnonymousClassInstance(an.srcPos, an.symbol, tmpl.body, parentCalls, ctor, t)) 
    case _ => None
  }
end AnonymousClassInstance 

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

// TODO: Need to try with a bigger project with multiple packages later on to see if we get the behaviour that we are expecting to get in terms of the classes that we generate.

// TODO: need to test with explicit evidence / our own custom type classes

// In the case of foo[S](a: Vec[S, Int, Int, Int, Int]) I think we ideally do want this because we should be able to get speed gains by accessing the specialized members 

// TODO: Only specialize if there is some material increase in specialization - I think only if at least one new parameter gets fully specialized
// Maybe it is better to not allow partial specializations --  we can think about that.

// TODO: Don't synthesize specialized instances for random generic types probably - as Hamza said we want to be able to control the specialization
