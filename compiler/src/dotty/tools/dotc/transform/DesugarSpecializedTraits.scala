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
import dotty.tools.dotc.typer.Typer
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Flags.GivenOrImplicit
import dotty.tools.dotc.core.NameKinds.ContextBoundParamName
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.transform.DesugarSpecializedTraits.isSpecializationOf
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.DesugarSpecializedTraits.isImplementationOf
import dotty.tools.dotc.core.Flags.InlineTrait
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.util.SrcPos
import dotty.tools.dotc.core.Decorators.nestedMap
import dotty.tools.dotc.core.NameOps.expandedName


class DesugarSpecializedTraits extends MacroTransform:

  override def phaseName: String = DesugarSpecializedTraits.name
  override def description: String = DesugarSpecializedTraits.description
  override def changesMembers: Boolean = false
  override def changesParents: Boolean = true 
  override def runsAfter: Set[String] =  Set("specializeInlineTraits")
  override def allowsImplicitSearch: Boolean = true

  override def run(using Context): Unit =
    try super.run
    catch case _: CompilationUnit.SuspendException => ()

  
  override def newTransformer(using Context): Transformer = new Transformer {

    private def newInterfaceTrait(specialization: Specialization, specializations: SpecializedTraitCache): (ClassSymbol, SpecializedTraitCache) = {
      val tm = new TypeMap: // TODO: Can we get this into the specialization ideally.
        def apply(t: Type) = specialization.specializedTypeParamsToTypeArgumentsMap.view.mapValues(_.tpe).applyOrElse(t, mapOver) // TODO: IF we can do just types we can get rid fo this 
    
      val inheritedParents = specialization.traitSymbol.denot.info.parents.filterNot(_ eq defn.ObjectType).map(tm(_))
      // Parents may be specializable and so we need to specialize them as well
      // See ArrayIterator extends Iterator in specialized-trait-collections-example.scala
      val specializations1 = inheritedParents.foldLeft(specializations)((specializations, parent) => 
          parent match {
            case Specialization(spec) if spec.isSpecialized => specializations.addInterface(spec) 
            case _ => specializations
          }
      )

      // Create new trait
      val parents = defn.ObjectType
                    :: AppliedTypeTree(Ident(specialization.traitSymbol.typeRef), specialization.specialization).tpe // original trait, specialized to Foo[Int]
                    :: inheritedParents.map(replaceSpecializedSymbolsMap(specializations1).typeMap(_))                                                                // parents of the original trait, specialized to Foo$sp$Int

      val traitSymbol = newNormalizedClassSymbol(
        specialization.traitSymbol.owner,
        DesugarSpecializedTraits.newSpecializedTraitName(specialization),
        Flags.Synthetic | Flags.Trait | Flags.Inline,
        parents,
        NoType, // TODO: What happens if the creator of the specialized inline trait provides a self type? 
        specialization.traitSymbol.privateWithin,
        specialization.traitSymbol.coord,
        specialization.traitSymbol.compilationUnitInfo
      )

      buildTypeParameters(traitSymbol, specialization)
      (traitSymbol.entered, specializations1)
    }

    private def buildInterfaceTraitTree(interfaceSymbol: ClassSymbol)(using Context) = {
      val init = newDefaultConstructor(interfaceSymbol)
      
      // TODO: Confirm that we don't need to worry about copying the evidence parameters over from the old constructor
      // These should be dealt with when we instantiate the original trait as a parent of this one. Otherwise we should be
      // able to copy them over, apply the specialization (keeping e.g. Numeric[Int] that arises from this) and 
      // pruning any that belong to Specialized.
      fixConstructor(init, interfaceSymbol)
      ClassDef(interfaceSymbol, DefDef(init.entered), Nil)
    }

    /* Fix constructor so that it:
        1) Has correct generic type parameters
        2) Returns the correct type corresponding to those type parameters applied */
    private def fixConstructor(init: Symbol, traitOrClassSymbol: ClassSymbol) = 
      val rt = traitOrClassSymbol.typeRef.appliedTo(traitOrClassSymbol.typeParams.map(_.typeRef))
      def resultType(tpe: Type): Option[Type] = tpe match {
          case mt @ MethodType(paramNames) => Some(mt.derivedLambdaType(paramNames, mt.paramInfos, resultType(mt.resultType).getOrElse(rt)))
          case pt : PolyType => Some(pt.derivedLambdaType(pt.paramNames, pt.paramInfos, resultType(pt.resType).get))
          case _ => None
      }
      init.info = resultType(init.info).get
      init.info = PolyType.fromParams(init.owner.typeParams, init.info)

    private def buildTypeParameters(traitOrClassSymbol: ClassSymbol, specialization: Specialization) =
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

    private def generateImplementationClassParents(specialization: Specialization, interfaceSymbol: ClassSymbol) = 
      val objectParent = defn.ObjectType
      val traitSpParent = interfaceSymbol.typeRef.appliedTo(specialization.unspecializedTypeParams) // Set using old unspecializedTypeParams and replace after.
      val originalTraitSpecializedParent = AppliedTypeTree(Ident(specialization.traitSymbol.typeRef), specialization.typeArguments).tpe
      (objectParent, traitSpParent, originalTraitSpecializedParent)

    private def newImplementationClass(specialization: Specialization, interfaceSymbol: ClassSymbol) =
      val (objectParent, traitSpParent, originalTraitSpecializedParent) = generateImplementationClassParents(specialization, interfaceSymbol)
      val parents = List(objectParent, traitSpParent, originalTraitSpecializedParent)

      val newImplementationClassSymbol = newNormalizedClassSymbol(
        specialization.traitSymbol.owner,
        DesugarSpecializedTraits.newImplementationClassName(specialization),
        Flags.Synthetic,
        parents,
        NoType, // TODO: What happens if the creator of the specialized inline trait provides a self type? 
        specialization.traitSymbol.privateWithin,
        specialization.traitSymbol.coord,
        specialization.traitSymbol.compilationUnitInfo
      )

      buildTypeParameters(newImplementationClassSymbol, specialization)

      newImplementationClassSymbol.entered

    // TODO: Do we want to share some code with the newSpecializedInterfaceTrait and buildInterfaceTraitTree?
    // TODO: Standardise a bit so that we either generate the symbols and later the classes or not.
    // TODO: Tidy this up a bit with functions
    private def buildImplementationClassTree(specialization: Specialization, interfaceSymbol: ClassSymbol, classSymbol: ClassSymbol)(using Context) = {
      val (objectParent, traitSpParent, originalTraitSpecializedParent) = generateImplementationClassParents(specialization, interfaceSymbol)
      val init = newDefaultConstructor(classSymbol)
      
      val tm = new TypeMap: // TODO: Can we get this into the specialization ideally.
        def apply(t: Type) = specialization.constructorParamToArgumentTypeMap.view.applyOrElse(t, mapOver) // TODO: IF we can do just types we can get rid fo this 
      
      val tm2 = new TypeMap:
          def apply(t: Type) = t match {
            case Specialization(spec) if spec.traitSymbol eq specialization.traitSymbol =>
              classSymbol.typeRef
            case _ => mapOver(t)
          }

      val nonTypeParams = specialization.traitSymbol.primaryConstructor.rawParamss.tail

      // We need to map the parameter names to avoid a name clash with val params from parents (see tests/pos/specialized-trait-val-parameter.scala)
      val valueParams = nonTypeParams.map(_.map(param => param.copy(owner = init, info = tm(param.info), name=param.name.expandedName(classSymbol)))) // .map(_.filterNot(isSyntheticEvidence)
      val typeParams = classSymbol.typeParams.map(_.copy())

      init.setParamss(typeParams :: valueParams)

      val paramAccessorss = valueParams.map(params => params.map(_.copy(owner = classSymbol, flags= Flags.LocalParamAccessor))) 
      paramAccessorss.foreach(_.foreach(classSymbol.enter(_)))

      init.info = tm2(specialization.traitSymbol.primaryConstructor.info.appliedTo(specialization.typeArguments.map(_.tpe)))

      fixConstructor(init, classSymbol)
      val typer = Typer(ctx.nestingLevel + 1) // TODO: actually get these from the user.
      
      val newParamss = 
        specialization.traitSymbol.primaryConstructor.paramSymss.tail.zip(paramAccessorss.map(_.map(ref))) // skip the type params
        .map((paramSyms, paramAccessors) =>
          paramSyms.zip(paramAccessors).map(
            (paramSym, accessor) =>
               if paramSym.name.asTermName.is(ContextBoundParamName) 
               then typer.implicitArgTree(tm(paramSym.info), paramSym.span) // TODO: Fix spans throughout
               else accessor
          )
        )

      val newParams1 = if (newParamss.length == 1) then newParamss ++ List(List()) else newParamss

      // TODO: Clean adn robust
      val classDef = ClassDefWithParents(
        classSymbol,
        DefDef(init.asTerm.entered), 
        List(
          New(objectParent, objectParent.classSymbol.primaryConstructor.asTerm, Nil),
          New(traitSpParent, traitSpParent.classSymbol.primaryConstructor.asTerm, Nil),
          New(originalTraitSpecializedParent.typeConstructor)
            .select(TermRef(originalTraitSpecializedParent.typeConstructor, specialization.traitSymbol.primaryConstructor.asTerm)) // TODO: Check for other constructors
            .appliedToTypes(originalTraitSpecializedParent.argTypes)
            // .appliedToArgss(paramAccessors.map(_.map(ref)))
            .appliedToArgss(newParams1)
            
            // TODO: What about potential custom typeclass instances? How do we balance that with generating another version of the class every time? Probably just generate the basic version and then let them apply their own version want (based on some kind of hashing). Then we generate a whole new impl class / or anon class which is still specialised to their instances that they provided, at the time that we see it?
            // To be honest if our assumption is that we aren't very often going to do anything weird we can just always generate the class at the point of use, with the evidences specialized (but only if we don't ahve that one already - i.e. effectively consider the evidences as part of the name) 
          ),
          // Put into body of class
          paramAccessorss.flatMap(syms => syms.map(sym => tpd.ValDef(sym.asTerm))) // .withFlags(Flags.LocalParamAccessor).withType(sym.info)
        )
      classDef
    }

    private def replaceSpecializedSymbolsMap(specializations: SpecializedTraitCache) =
      val typeMap = new TypeMap:
        def apply(t: Type) = t match {
          case Specialization(spec) => 
            {
              for (specializedSymbol <- specializations.getInterfaceSymbol(spec))
              yield specializedSymbol.typeRef.appliedTo(spec.unspecializedTypeArgs.map(_.tpe))
            }.getOrElse(mapOver(t))
          case _ => mapOver(t)
        }

      def treeMap(tree: Tree): Tree = tree match {
        // Replace (anonymous class version of) new Foo[Int] {} with new Foo$impl$Int.asInstanceOf[Foo$sp$Int] 
        case Block(List(an@TypeDef(anon, Template(_, parentCalls: List[Tree], _, _))),  
                  Typed(Apply(Select(New(anon1),ctor), _), t: TypeTree)) if anon1.symbol.isAnonymousClass =>
          parentCalls match {
            case _ :+ Apply(Apply(tpe, ctorArgs), ev) => // extends Object, parents of spec trait, spec trait
              val spec = Specialization.unapply(t.tpe).get
              { // We don't replace non-specialized anonymous class instantiations e.g. new Foo[T] where T is defined in the enclosing scope.
                for (specializedSymbol <- specializations.getImplementationSymbol(spec))
                yield Typed(Apply(Apply(Select(New(ref(specializedSymbol)),ctor).appliedToTypeTrees(spec.unspecializedTypeArgs), ctorArgs.map(_.changeNonLocalOwners(an.symbol.owner))), ev), t)
              }.getOrElse(tree)
            case _ => tree
          }

        // Replace class Bar extends Foo[Int](params) with class Bar extends Foo$sp$Int(params)
        // TODO: Why do we still have this case if we don't allow this pattern? 
        // Note: We always drop the evidence params when creating these new specialized traits so we know that there are none, but we may need to revisit this if we decide we do want to copy the evidence parameters over
        case Apply(TypeApply(fun@Select(New(tpt), init), args), ev) if fun.symbol.isConstructor => 
          val spec = Specialization(fun.symbol.owner, args)
          {
            for (specializedSymbol <- specializations.getInterfaceSymbol(spec))
            yield New(ref(specializedSymbol)).select(init).appliedToTypeTrees(spec.unspecializedTypeArgs)
          }.getOrElse(tree)

        // Replace AppliedTypeTree instances in code
        case Specialization(spec) => {
          for (specializedSymbol <- specializations.getInterfaceSymbol(spec))
          yield AppliedTypeTree(Ident(specializedSymbol.typeRef), spec.unspecializedTypeArgs) // TODO: Matching on a Specialization and then outputting ATT is weird - maybe have a method on specialization to convert to ATT .toAppliedTypeTree?
        }.getOrElse(tree)

        /* case sel@Select(qualifier, name) if typeMap(sel.denot.info) != sel.denot.info => 
          Select(qualifier, name ++ str.SPECIALIZED_METHOD_TARGET_NAME_SUFFIX) */
        case tree => tree
      }
      
      // TODO: Do we acvtually need to worry about these cases if we have enough limitations?
      new TreeTypeMap(typeMap, treeMap) {
        override def transform(tree: Tree)(using Context): Tree = tree match { // HACK: This seems to do what we want but I don't understand why we don't do this by default? Surely we should apply transformDefs over template body?
          case dd@DefDef(name, paramss, tpt, preRhs) => 
            val transformedDef = super.transform(dd)
            transformedDef.symbol.info = mapType(transformedDef.symbol.info)
            if transformedDef.symbol.allOverriddenSymbols.isEmpty then
              transformedDef.symbol.flags = transformedDef.symbol.flags &~ Flags.Override
            transformedDef

          case vd@ValDef(name, tpt, preRhs) => 
            val transformedDef = super.transform(vd)
            transformedDef.symbol.info = mapType(transformedDef.symbol.info)
            if transformedDef.symbol.allOverriddenSymbols.isEmpty then
              transformedDef.symbol.flags = transformedDef.symbol.flags &~ Flags.Override
            transformedDef
          /*case vd@ValDef(name, tpt, preRhs) =>
            val transformedDef = super.transform(vd).asInstanceOf[ValDef]
            if transformedDef.symbol.info != mapType(transformedDef.symbol.info) && transformedDef.symbol.allOverriddenSymbols.nonEmpty then
              val specializedSymbol = newSymbol(
                transformedDef.symbol.owner,
                transformedDef.symbol.name ++ str.SPECIALIZED_METHOD_TARGET_NAME_SUFFIX,
                transformedDef.symbol.flags &~ Flags.Override,
                info = mapType(transformedDef.symbol.info),
                transformedDef.symbol.privateWithin,
                transformedDef.symbol.coord,
                transformedDef.symbol.nestingLevel
              ).entered
              ValDef(specializedSymbol.asTerm, transformedDef.rhs.changeOwner(transformedDef.symbol, specializedSymbol))
            else
              transformedDef
          
          case dd@DefDef(name, paramss, tpt, preRhs) => 
            val transformedDef = super.transform(dd).asInstanceOf[DefDef]
            
            if transformedDef.symbol.info != mapType(transformedDef.symbol.info) && transformedDef.symbol.allOverriddenSymbols.nonEmpty then
              val specializedSymbol = newSymbol(
                transformedDef.symbol.owner,
                transformedDef.symbol.name ++ str.SPECIALIZED_METHOD_TARGET_NAME_SUFFIX,
                transformedDef.symbol.flags &~ Flags.Override,
                info = mapType(transformedDef.symbol.info),
                transformedDef.symbol.privateWithin,
                transformedDef.symbol.coord,
                transformedDef.symbol.nestingLevel
              ).entered

              val rhsFun: List[List[Tree]] => Tree = paramss =>
                val oldParamSyms = transformedDef.paramss.flatten.map(_.symbol)
                val newParamSyms = paramss.flatten.map(_.symbol)
                transformedDef.rhs.subst(oldParamSyms, newParamSyms).changeOwner(transformedDef.symbol, specializedSymbol)

              DefDef(specializedSymbol.asTerm, rhsFun)
            else
              transformedDef*/

          case impl@Template(constr, preParentsOrDerived, self, _) => 
            impl.parents.foreach(p => 
              p.tpe match {
               case Specialization(spec) if 
                spec.hasSpecializedParams 
                && !impl.symbol.owner.isAnonymousClass // impl.symbol = the dummy class; owner is the actual class.
                && !isSpecializationOf(impl.symbol.typeRef, p.tpe, allowImplementationClass = true)
                && !isImplementationOf(impl.symbol.owner.name, p.tpe.typeSymbol.name)
                && !impl.symbol.owner.isOneOf(InlineTrait) =>
                  report.error("Specialized traits may only be extended by anonymous class instances or inline traits.", impl.srcPos)
                case _ => 
              }
            )

            /*
            // If a class has a specialized member which was overriding a parent member, this override is lost because we specialize the types.
            // E.g. def foo(Vec$sp$Int) cannot override def foo(Vec[Int]) because signatures must match exactly for overriding.
            // However, specialized trait is based on the invariant that ∀T. T <: Foo[Int] => T <: Foo$sp$Int (and note that the reverse <= holds trivially by inheritance).
            // This means it is safe to build bridge methods which simply apply the relevant casts so that we satisfy the interface, although we don't expect to call these.
            def isMapped(t: Type) = mapType(t) != t
            */
            val mappedbody = impl.body.map(transform(_))
            
            /*
            val bridgeMethods = impl.body.collect {
              case ddef@DefDef(name, paramss, _, _) if ddef.symbol.allOverriddenSymbols.nonEmpty && isMapped(ddef.symbol.info) => 
                // Any callers of the original method will have been redirected to the bridge method because it has a signature match with the method they were calling            
                val ddef2 = cpy.DefDef(ddef)(
                  rhs=
                    This(impl.symbol.owner.asClass).select(ddef.symbol.name ++ str.SPECIALIZED_METHOD_TARGET_NAME_SUFFIX)
                    .appliedToArgss(
                      ddef.termParamss.map(
                        params => params.map(p => 
                          ref(p.symbol).cast(mapType(p.symbol.info)))
                      )
                    ).cast(ddef.symbol.localReturnType)
                )
                ddef2.symbol.rawParamss = ddef2.paramss.nestedMap(_.symbol)
                ddef2
                
              case vdef: ValDef if vdef.symbol.allOverriddenSymbols.nonEmpty && isMapped(vdef.symbol.info) => 
                cpy.ValDef(vdef)(
                  rhs = This(impl.symbol.owner.asClass).select(vdef.symbol.name ++ str.SPECIALIZED_METHOD_TARGET_NAME_SUFFIX).cast(vdef.symbol.info)
                )
            } */
           
            /* We need to map parents of non-specialized inline traits (see tests/pos/specialized-trait-partial-complete-specialization-with-return-type.scala, we need 
            to map the A[Int] reference to A$sp$Int in B's parents) */
            val mappedparents = impl.parents.map(transform(_))
            val oldInfo = impl.symbol.owner.info.asInstanceOf[ClassInfo]
            impl.symbol.owner.info = oldInfo.derivedClassInfo(declaredParents = oldInfo.declaredParents.map(mapType(_)))

            cpy.Template(impl)(body = mappedbody, parents = mappedparents)
          case tree => super.transform(tree)
        }
      }
    end replaceSpecializedSymbolsMap

    // Returns (new stmts including original, new symbols including original)
    private def transformStatements(stats: List[Tree], span: Span, specializations: SpecializedTraitCache): (List[Tree], SpecializedTraitCache) = {
      val specializations1 = collectReferencedSpecializations(stats, specializations)
      val generatedTraitStats = specializations1.getNewInterfaceSymbols.toList.map(buildInterfaceTraitTree)
      val generatedClassStats = specializations1.getNewImplementationSymbols.toList.map(buildImplementationClassTree)

      val specializations2 = specializations1.installNewInterfaceSymbols.installNewImplementationSymbols

      // We have Vec$sp$Int extends Vec[Int] in order to do the inlining, but then remove this parent 
      // afterwards to avoid interface implementation problems (see tests/run/specialized-trait-as-parameter.scala,
      // tests/run/specialized-trait-as-return-type.scala)
      extension (classTree: Tree)
        def updateParents(parentUpdater: List[Type] => List[Type]) = (classTree: @unchecked) match {
          case td@TypeDef(name, t@Template(constr, preParentsOrDerived, self, preBody)) =>  
          td.symbol.info = td.symbol.info match {
            case ci: ClassInfo => ci.derivedClassInfo(declaredParents=parentUpdater(ci.declaredParents)) 
          }
          ClassDef(td.symbol.asClass, constr, t.body)
        }

      /* We need to inline recursively throughout generated specialized traits - see tests/run/specialized-trait-requires-inline-trait-inlining.scala */
      // TODO: How do we calculate the spans correctly?
      val ttmap = new TreeTypeMap(treeMap = {
        case tree: TypeDef if tree.symbol.isInlineTrait =>
          val tree1 = Inlines.transformInlineTrait(tree)
          val tree2 = if Inlines.needsInlining(tree1) then Inlines.inlineParentInlineTraits(tree1) else tree1
          tree2
        case tree: TypeDef if Inlines.needsInlining(tree) =>
          Inlines.inlineParentInlineTraits(tree)
        case t => t
      })
      
      // Why does it cause no denotation to happen?
      val generatedTraitStats1 = generatedTraitStats.map(trtDef => /*Inlines.inlineParentInlineTraits(Inlines.transformInlineTrait(*/ttmap(trtDef.withSpan(span))/*))*/).map:
        _.updateParents { parents => (parents: @unchecked) match
          case obj :: original :: parents => obj :: parents
        }
  
      val generatedClassStats1 = generatedClassStats.map(clsDef => /*Inlines.inlineParentInlineTraits(*/ttmap(clsDef.withSpan(span))/*)*/).map:
        _.updateParents { parents => (parents: @unchecked) match
          case obj :: traitSp :: originalSpec :: Nil => obj :: traitSp :: Nil 
        }
      
      if (generatedTraitStats1.isEmpty && generatedClassStats1.isEmpty)
        (stats.map(replaceSpecializedSymbolsMap(specializations2)(_)), specializations2)
      else 
        val (generatedTraitStats2, specializations3) = transformStatements(generatedTraitStats1, span, specializations2)
        val (generatedClassStats2, specializations4) = transformStatements(generatedClassStats1, span, specializations3)
        (generatedTraitStats2 ++ generatedClassStats2 ++ stats.map(replaceSpecializedSymbolsMap(specializations4)(_)), specializations4)
    }

    override def transform(tree: Tree)(using Context): Tree = tree
       match { // TODO: Is Package level processing really what we want? Given we are not going to output the classes somewhere else do we not really want either to deepFold the whole tree directly or do a more direct transform?
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
          
          val (stats1, _) = transformStatements(stats, tree.span, SpecializedTraitCache(genInterfaceSymbol = newInterfaceTrait, genImplementationSymbol = newImplementationClass)) // TODO: Fix span
          cpy.PackageDef(pkg)(pid, stats1)
      }

    // TODO: There is a case where recursive expansion causes something to need an implementation where it didn't before.


    // TODO: Try with just generating new Foo(100) with no function to pass it to and no other references to Foo. this may not work because we might not
    // correctly detect it. 

    private def collectReferencedSpecializations(stats: List[Tree], specializations: SpecializedTraitCache)(using Context): SpecializedTraitCache =
      stats.foldLeft(specializations)((specializations, tree) => {
        tree.deepFold(specializations)((specializations, tree) => tree match
          case Typed(Apply(Select(New(anon),ctor),List()), t: TypeTree) if anon.symbol.isAnonymousClass =>
            t.tpe match {
              case Specialization(spec) if spec.isSpecialized => specializations.addInterfaceAndImplementation(spec)
              case _ => specializations
            }
          case Specialization(spec) => 
            if (spec.isSpecialized) {
              // Block Vec[?] and similar
              spec.specializedTypeArgs.filter {
                case t: TypeBoundsTree => true
                case _ => false
              }.foreach: tr => 
                  report.error("Wildcard types may not be substituted for Specialized type parameters.", tr.srcPos)
              
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
          case _ => specializations
        )
      })
  }
end DesugarSpecializedTraits
                  // TODO: Need to think carefully about the behaviour when we are integrating libraries - should the library generate the implementation classes or the user?
                  // In any case we need to read back in either the $sp$ classes or the $impl$ traits to be able to work with them.              

object DesugarSpecializedTraits:
  val name: String = "desugarSpecializedTraits"
  val description: String = "Replaces traits having type parameters that have the Specialized annotation with specialized versions"

  // TODO: What happens with this name generation if we have Vec[Vec[T]] for example? We potentially don't have an Ident
  // TODO: Check what happens here when we have a case where the types being specialized into are user defined instead of primitives or type vars.
  private def generateName(specialization: Specialization, suffix: String)(using Context) = // TODO: Probably don't use show
    specialization.specializedTypeArgs.collect(t => t.tpe.show ++ str.SPECIALIZED_TRAIT_TYPE_SEP).foldLeft((specialization.traitSymbol.name ++ suffix).asTypeName)((n1, n2) => n1 ++ n2)

  private[transform] def newSpecializedTraitName(specialization: Specialization)(using Context): TypeName = 
    generateName(specialization, str.SPECIALIZED_TRAIT_SUFFIX)

  private[transform] def newImplementationClassName(specialization: Specialization)(using Context): TypeName = 
    generateName(specialization, str.SPECIALIZED_TRAIT_IMPL_SUFFIX)

  // TODO: Put this somewhere else; consider if we want to do it like this?
  def isSpecializationOf(type1: Type, type2: Type, allowImplementationClass: Boolean = false)(using Context) = 
    type2 match {
      case Specialization(spec) => type1 match {
        case AppliedType(tp, args) =>
          tp.typeSymbol.name == newSpecializedTraitName(spec)
          || (allowImplementationClass && tp.typeSymbol.name == newImplementationClassName(spec))
        case tp: TypeRef => 
          (tp.typeSymbol.name.toString.contains(newSpecializedTraitName(spec).toString) && 
          tp.symbol.owner.name == newSpecializedTraitName(spec))
          || 
          (allowImplementationClass &&
          tp.typeSymbol.name.toString.contains(newImplementationClassName(spec).toString) && 
          tp.symbol.owner.name == newImplementationClassName(spec)
          )
        case _ => false
      }
      case _ => false
    }

  // TODO: Maybe make consistent with the isSpecializationOf function
  def isImplementationOf(name1: Name, name2: Name)(using Context) = 
    name1.toString().replace(str.SPECIALIZED_TRAIT_IMPL_SUFFIX, str.SPECIALIZED_TRAIT_SUFFIX) == name2.toString()

end DesugarSpecializedTraits
/*
  Stores the specializations we have found in the program and the symbols for the interface traits and implementation classes
  that will replace them. We generate these symbols when we enter the specializations into the cache, via the functions
  we store in genInterfaceSymbol and genImplementationSymbol. 

  Model: Contains two levels:
           - interface/implementation symbols we have found since the last installNewInterface/ImplementationSymbols call
            (i.e. typically on this iteration) ("new")
           - Those we found prior to that call

  Invariant: (newImplementationSymbols \cup implementationSymbols) \subseteq (interfaceSymbols \cup newInterfaceSymbols).
    This is enforced by only providing addInterface and addInterfaceAndImplementation, and allows the unchecked get in 
    getNeWImplementationSymbols.

*/

object SpecializedTraitCache:
  type SymbolMap = Map[Specialization, ClassSymbol]
  type GenInterfaceSymbol = (Specialization, SpecializedTraitCache) => (ClassSymbol, SpecializedTraitCache)
  type GenImplementationSymbol = (Specialization, ClassSymbol) => ClassSymbol


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

  def getNewInterfaceSymbols = newInterfaceSymbols.values 
  def getNewImplementationSymbols: List[(Specialization, ClassSymbol, ClassSymbol)] = newImplementationSymbols.map((k, v) => (k, getInterfaceSymbol(k).get, v)).toList

  def addInterface(spec: Specialization): SpecializedTraitCache = 
    if (newInterfaceSymbols.contains(spec) || interfaceSymbols.contains(spec)) then
      this
    else
      val (targetSymbol, resultingCache) = genInterfaceSymbol(spec, this)
      resultingCache.copy(newInterfaceSymbols = resultingCache.newInterfaceSymbols + (spec -> targetSymbol))
  def addInterfaceAndImplementation(spec: Specialization): SpecializedTraitCache = 
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
class Specialization(val traitSymbol: Symbol, val typeArguments: List[Tree])(using Context): // TODO: Can we get away with List[Type]
  val specializedTypeParams: List[Type] = Specialization.classSpecializedTypeParams(traitSymbol) // Type parameters marked with Specialized
  
  private val specializedTypeParamsSet = specializedTypeParams.toSet
  private val paramToArgList = traitSymbol.typeParams.map(_.typeRef.asInstanceOf[Type]).zip(typeArguments)

  val unspecializedTypeParams: List[Type] = paramToArgList.filterNot((tParam, tArg) => specializedTypeParamsSet(tParam)).map(_._1) // Type parameters not marked with Specialized
  val specializedTypeArgs: List[Tree] = paramToArgList.filter((tParam, tArg) => specializedTypeParamsSet(tParam)).map(_._2) // Type arguments provided to parameters that are marked with Specialized at their definition
  val unspecializedTypeArgs: List[Tree] = paramToArgList.filterNot((tParam, tArg) => specializedTypeParamsSet(tParam)).map(_._2) // Type arguments provided to parameters that are not marked with Specialized at their definition 

  val specializedTypeParamsToTypeArgumentsMap: Map[Type, Tree] = paramToArgList.toMap.filter((k, v) => specializedTypeParamsSet(k))
  val specialization: List[Tree] = traitSymbol.typeParams.map(_.typeRef).map(specializedTypeParamsToTypeArgumentsMap.applyOrElse(_, TypeTree(_))) // TODO: Don't really like this name
  // val constructorParamToArgumentTypeMap: Map[Type, Type] = traitSymbol.primaryConstructor.typeParams.zip(paramToArgList).filter((constrParam, paramArg) => specializedTypeParamsSet(paramArg._1)).map((constrParam, paramArg) => (constrParam.typeRef, paramArg._1)).toMap

  // TODO: Potentially can get this out of the specialization.specialization directly given we make the same assumption about one primary constructor and param ordering. 
  def constructorParamToArgumentTypeMap: Map[Type, Type] = 
    traitSymbol.primaryConstructor.rawParamss.head.map(_.typeRef).zip(typeArguments.map(_.tpe)).toMap

  def hasSpecializedParams: Boolean = specializedTypeParams.nonEmpty

  // If inline trait Foo[T] has a method taking another Foo[T] there's no point specializing the reference
  // since the resulting sp$T$ would be the same as the starting trait.
  def isSpecialized: Boolean = 
    hasSpecializedParams && typeArguments.exists(!_.tpe.existsPart(_.typeSymbol.isTypeParam)) //) !tpt.symbol.isTypeParam) //  .zip(traitSymbol.typeParams).forall((t, s) => t.tpe =:= s.typeRef))

  // Note: We only care about the specialized arguments for equality; a specialization of Vec[A: Specialized, B] with B = Int and one
  // with B = String can be considered to be the same as they use the same specialized trait
  // TODO: I don't really like this logic being in Specialization because they are really different
  // We should really put that logic in the SpecializedTraitCache because it's at that point that we treat them as the same.
  override def equals(obj: Any): Boolean = 
    obj.isInstanceOf[Specialization] && obj.asInstanceOf[Specialization].traitSymbol == traitSymbol
    && specializedTypeArgs.zip(obj.asInstanceOf[Specialization].specializedTypeArgs).forall((a1, a2) => a1.tpe =:= a2.tpe)

  override def hashCode(): Int = 
    (traitSymbol, specializedTypeArgs.map(_.tpe.widen.dealias.show)).hashCode() // TODO: Consider not using show for this for performance reasons (correctness also?)
end Specialization

object Specialization:
  object SpecializedEvidence {
    def unapply(tpe: Type)(using Context): Option[Type] = tpe match {
      case AppliedType(tycon, List(tpeArg)) if (tycon =:= ctx.definitions.SpecializedClass.typeRef && tpeArg.typeSymbol.isTypeParam) => Some(tpeArg)
      case _ => None
    }
  }

  def unapply(tpt: Tree)(using Context): Option[Specialization] = tpt match {
    case AppliedTypeTree(specializedTrait: Ident, concreteTypeTrees: List[Tree]) => Some(Specialization(specializedTrait.denot.symbol, concreteTypeTrees))
    case t: TypeTree => Specialization.unapply(t.tpe)
    case _ => None
  }
  
  def unapply(tpe: Type)(using Context): Option[Specialization] = tpe match {
    case AppliedType(tycon: Type, args: List[Type]) => Some(Specialization(tycon.typeSymbol, args.map(TypeTree(_))))
    case _ => None
  }

  def classSpecializedTypeParams(classSym: Symbol)(using Context): List[Type] = classSym.unforcedDecls.implicitDecls.collect(_.info match { case SpecializedEvidence(typeVar) => typeVar })
  
  def anonymousClassIsSpecialized(tree: Tree)(using Context) = tree match {
    case TypeDef(anon, Template(_, parentCalls: List[Tree], _, _)) =>
      parentCalls match {
        case _ :+ Apply(Apply(t@tpe, ctorArgs), ev) => // extends Object, parents of spec trait, spec trait
          val spec = Specialization.unapply(t.tpe.resultType.resultType)
          spec.get.hasSpecializedParams
        case _ => true
      }
    case _ => true
  } 
end Specialization

// Would be nice to define a Specialization class I think
//   -> Map the specialized type params to Int etc
//   -> Map the non-specialized type params to new type params
//   -> Be a canonical representation so we can store that in a set
//   -> Generate a name / string representation for use in new traits
//   -> Get the specialized list to apply


// TODO: Fix name generation which doesn't work if the tpye isn't provided explicitly




// Generate impl instead of generating anonymous classes every time to avoid insane code bloat
  // Do we really want the method definitions to live in the implementation classes or in the trait?|
  // I think in the trait is fine but note that this only actually saves any space if we don't use anonymous classes (because those copy parent members automatically it seems)
// Need to make sure all my examples are up to date, consistent with what we do and what we want to do so that they are actually useful for the future.
// Need to somehow make my naming a lot more consistent as well.
// Correctly generate names
// generate classes as well
// do we actually want to generate Iteratorsp$Int
// should we be worried about the results that we generate causing more stuff to be generated?
// figure out why we generate the T version.
// Try to see if we can do with only types and not trees
// Synthesise Specialized instances so that people can't do stupid stuff like Specialized[Array[T]]. type x = Specialized[Array[Array[Int]]]
// Set the Synthetic flags somewhere
// Cache / only generate once instead of multiple times.
// Ideally standardise on either specialization or specializationMap

// TODO: Think carefully about use of primaryConstructor and the other appropriateConstructors call or whatever it was.

// Probably (tree)typemap

// Would it be better to just copy rather than creating everything from scratch? I think this is right

// 1. Figure out which specializations we need to generate
// 2. Generate ArrayIterator$sp$Int and ArrayIterator$impl$Int wherever they live
// 3. Replace ArrayIterator[Int] with ArrayIterator$sp$Int
// 4. Replace new ArrayIterator[Int](xs) {} with new ArrayIterator$impl$Int(xs) {}
// 5. Somehow figure out the caching
// 6. Delete references to Specialized I guess

// Synthesize Specialized[T] instances.
// TODO: Need to try with a bigger project with multiple packages later on to see if we get the behaviour that we are expecting to get in terms of the classes that we generate.

// Need to ban all of these but we will do that earlier I guess?
// Vec[Vec[Int]] hehe <- fine
// Vec[S, S[T]: Specialized] <- banned
// Vec[S, T[T]: Specialized] <- banned
// Vec[Array[T]: Specialized] <- banned


// TODO: Prune the generated anonymous classes.


// need to test with explicit evidence / our own custom type classes
// TODO: Make sure name encoding is fully qualified - e.g. potential for conflicts if we define our own class Int.
  // // TODO: check that we have a single type var only

// trait Vec$Sp[S] extends Vec[S, Int, Int, Int, Int]
// inline trait Two[S: Specialized] extends Vec$sp[S]
// does mean that any methods in the original trait lose their specialization - maybe we /should/ make the generated traits inline?
// hmm but we can't do that because we need the methods called on the Vec$Sp trait to be the specialized ones - that is really important. 
// Could potentially copy over the inline based on whether Two is inline or not? Needs some thought.


// In the case of foo[S](a: Vec[S, Int, Int, Int, Int]) I think we ideally do want this because we should be able to get speed gains by accessing the specialized members 

// Should we allow these? I think they are all fine
// inline trait Two[S: Specialized] extends Vec[S, Int, Int, Int, Int]
// inline trait Two[S] extends Vec[S, Int, Int, Int, Int] // Maybe worth warning? Perhaps behind an extra flag
// trait Two[S] extends Vec[S, Int, Int, Int, Int]
// TODO: We want a self reference case where Vec[T] has some method that takes a Vec[Int] for example.
// TODO: Fix broken "inline" tests 
// TODO: Only specialize if there is some material increase in specialization - I think only if at least one new parameter gets fully specialized
// Maybe it is better to not allow partial specializations --  we can think about that.
// TODO: Need to add the rule that we need to directly extend inline traits to pass parameters in the same way as for normal traits
// See inline-trait-param-no-shadow.scala

// could ban normal traits from mixing in inline traits on the basis that 
// it's a very unlikely usecase and would fix the mixin problem - although
// we could just try and fix the mixin problem properly.

// TOOD: probably need to do this from Timothée:
  // - private members are not renamed, only private parameter accessors are;
// this needs to be changed so that all overridable private members are renamed
// Implement postphase checks for inline traits
// TODO: Don't synthesize specialized instances for random generic types probably - as Hamza said we want to be able to control the specialization
// TODO: If we are to ban trait extends inline trait then need to fix some tests.
// TODO: Check that when we extend parents we actually do so in the specialized forms.
// TODO: Fix specialized-trait-collections-example.scala
// TODO: Test extensively with inline methods and inline traits.

// TODO:
  // Need  to enforce these:
//  - can extend only a single specialized trait,
//  - cannot mix in further classes or traits, and
//  - cannot contain member definitions.
// I'm not sure we strictly need them though.

// TODO: Put classes onto the classpath as desired.

// Concerns:
//  - The superclass of `C` is a top class, or `C` itself is a top class.
// Drop all specialized trait parameters of A

// If we can manage to get rid of the inheritance there that could be helpful in terms of avoiding multiple values
// BUT: generate a version which is with just inline traits that has this problem as well.
// Need to deal with the caching at some point
// These implementation classes are type correct as long as we inject the knowledge that a specialization trait
// like `Seq$sp$Int` is equal to its parameterized version `Seq[Int]`

// Warning for dropping Specialized qualifier or it doesn't compile?
// TODO: Make name consistent for tests.
// TODO: In order to fix Foo extends Bar (banned for now)
// //           case impl@Template(constr, preParentsOrDerived, self, _) => 
//             cpy.Template(impl)(body = impl.body.map(transform(_)), 
//             parents = // CAN POTENTIALLY MOVE THE OWNER CALL UP HERE. 
//               impl.parents.map(p => if isSpecializationOf(impl.symbol.typeRef, p.tpe, allowImplementationClass = true) then {println(impl.symbol.typeRef); println(p.tpe); p} else transform(p)))
//           case tree => super.transform(tree)
//         }
// Plus need another case in the normal map where you add sp trait as a parent as well as the original trait, AND update symbols. (or maybe switch to impl calss also possibhle).

// end DesugarSpecializedTraits
// Also delete the other members that already got inlined or maybe we don't care.
// extend both traits
