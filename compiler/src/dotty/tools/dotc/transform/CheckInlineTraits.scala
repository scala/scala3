package dotty.tools.dotc.transform

import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.{IllegalUseOfSpecialized, VarianceInSpecializedTraitsLimitation}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.NameKinds.ContextBoundParamName
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.transform.Specialization.isSpecializationCandidate
import dotty.tools.dotc.transform.Specialization.isSpecializedTrait
import dotty.tools.dotc.util.SrcPos
import dotty.tools.dotc.reporting.VarianceInSpecializedTraitsLimitation

object CheckInlineTraits:
  val name: String = "checkInlineTraits"
  val description: String = "check if inline & specialized traits are present and are correctly used"

class CheckInlineTraits extends Phase:
    override def phaseName: String = CheckInlineTraits.name 

    override def description: String = CheckInlineTraits.description

    override def runsAfter: Set[String] = Set("typer")

    override def isCheckable: Boolean = false 
    
    override protected def run(using Context): Unit =
      val unit = ctx.compilationUnit
      inlineTraitsChecker.traverse(unit.tpdTree)
      specializedChecker.traverse(unit.tpdTree)

    private val inlineTraitsChecker = new TreeTraverser:
      def traverse(tree: Tree)(using Context): Unit =
        traverseChildren(tree)
        
        tree match
          case tree: Ident if !tree.name.isTypeName && tree.symbol == defn.SpecializedModule_apply =>
            registerSpecializationsInUnit()

          case tree: Template if tree.parents.exists(p => Inlines.symbolFromParent(p).isInlineTrait) =>
            registerSpecializationsInUnit()
         
          case tree: TypeDef => 
            checkInlineTraitParents(tree)
            if tree.symbol.isInlineTrait then 
              registerSpecializationsInUnit()
           
          case tree: Block => 
            checkInlineAnonymousClass(tree)
          
          case tree: (ValDef | DefDef) => 
            checkInlineTraitPrivateMemberIsLocal(tree.symbol, tree.srcPos)
          
          case _ =>

    private def checkInlineAnonymousClass(tree: Block)(using Context): Unit = tree match
      case AnonymousClassInstance(anon) =>
        anon.typeTree.tpe match
          case _: AndType => // Checked in checkBlock
          case tpe =>
            if Specialization.unapply(tpe, anon.typeTree.span).exists(_.hasSpecializedParams) then
              registerSpecializationsInUnit()
      case _ =>
    
    private def checkInlineTraitPrivateMemberIsLocal(sym: Symbol, srcPos: SrcPos)(using Context): Unit =
      if sym.exists && !sym.is(Synthetic) && sym.owner.isInlineTrait && sym.isAllOf(Private, butNot = Local) then
        report.error(
          """
          implementation restriction: inline traits cannot have non-local private members.
          This also means no retained inline methods.
          """, 
          srcPos
        )
  
          
    private def checkInlineTraitParents(tree: TypeDef)(using Context): Unit =
      val sym = tree.symbol
      if sym.isClass && sym.isAllOf(Trait, butNot = Inline) then
        def constructorHasTermParam(sym: ClassSymbol): Boolean = 
          sym.primaryConstructor.paramSymss
            .exists(paramList => paramList.nonEmpty && paramList.head.isTerm)

        val problemParents = sym.info.parents
          .filter { 
            parent => parent.classSymbol match 
              case NoSymbol => 
                false
              case classSymbol: ClassSymbol => 
                classSymbol.isInlineTrait && constructorHasTermParam(classSymbol)
          }

        problemParents.foreach { parent =>
          val message = s"""
            Only parameterless inline traits may be extended by ordinary traits. 
            Make $sym inline or remove inline ${parent.typeSymbol}'s parameter list.
          """

          report.error(message, tree.srcPos)
        }

    private val specializedChecker = new TreeTraverser:
      def traverse(tree: Tree)(using Context): Unit =
        traverseChildren(tree)
        
        tree match
          case tree: AppliedTypeTree => checkSpecializedAppliedTypeTree(tree)
          case tree: Ident           => checkSpecializedIdent(tree)
          case tree: DefDef          => checkSpecializedDefDef(tree)
          case tree: TypeDef         => checkSpecializedTypeDef(tree)
          case tree: Block           => checkSpecializedAnonClass(tree)
          case _                     => ()

        if !ctx.compilationUnit.hasSpecializations then return 
        
        tree match
          case Typed(Apply(Select(New(anon), _), Nil), _: TypeTree) if anon.symbol.isAnonymousClass => ()
          case Specialization(specialization) => checkSpecializationTypeArgs(specialization)
          case _                              => ()

    private def checkSpecializedIdent(tree: Ident)(using Context): Unit =
      if !tree.name.isTypeName then
        val sym = tree.symbol
        if sym == defn.SpecializedModule && (ctx.owner ne defn.SpecializedModule.moduleClass) then
          report.error(IllegalUseOfSpecialized(), tree.srcPos)

    private def checkSpecializedDefDef(tree: DefDef)(using Context): Unit = {
      val sym = tree.symbol
      val isConstructorOfNonInlineType = sym.isConstructor && !sym.owner.is(Inline)
      val isRegularDefDef = !sym.isConstructor && !sym.is(Inline) 
      if isConstructorOfNonInlineType || isRegularDefDef then
        tree.paramss.flatten.foreach {
          param => if SpecializedEvidence.unapply(param.tpe.widen.dealias).nonEmpty then 
            report.error("Only inline traits and inline functions may take Specialized type parameters", param.srcPos)
        }
    }

    private def checkSpecializedAppliedTypeTree(tree: AppliedTypeTree)(using Context): Unit = {
      val sym = tree.tpt.symbol
      if sym != defn.orType && sym != defn.andType && sym == defn.SpecializedClass && 
          !(ctx.owner.name.is(ContextBoundParamName) || ctx.owner.ownersIterator.contains(defn.SpecializedModule_apply)) then
        report.error(IllegalUseOfSpecialized(), tree.srcPos)
    }

    private def checkSpecializedTypeDef(tree: TypeDef)(using Context): Unit = {
      val containUseOfSpecialized = 
        tree.rhs.tpe.existsPart(t => t.typeSymbol == defn.SpecializedClass.asType) 
      if containUseOfSpecialized && (tree.symbol ne defn.SpecializedClass) then
        report.error(IllegalUseOfSpecialized(), tree.srcPos)
      
      val symbol = tree.symbol
      if tree.rhs.isInstanceOf[Template] && symbol.isSpecializedTrait then
        checkSpecializedTraitDef(tree)
    }

    private def checkSpecializedTraitDef(tree: TypeDef)(using Context): Unit = {
      val symbol = tree.symbol
      if !symbol.isStatic then
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
          symbol.srcPos
        )

      symbol.typeParams.foreach { param =>
        val isVariant = param.paramVariance.isOneOf(Flags.Contravariant | Flags.Covariant) && 
          Specialization.classSpecializedTypeParams(symbol).exists(tpe => tpe.typeSymbol == param)
        if isVariant then
          report.warning(VarianceInSpecializedTraitsLimitation(), param.srcPos)
      }
    }

    private def checkSpecializedAnonClass(tree: Block)(using Context): Unit = tree match {
      case AnonymousClassInstance(anon) =>
        def deandify(tp: Type): Iterator[Type] = tp match {
          case AndType(l, r) => deandify(l) ++ deandify(r)
          case _ => Iterator.single(tp)
        }

        def checkAnonSpecializedMixins(tpe: AndType): Unit = {
          for 
            t <- deandify(tpe);   
            spec <- Specialization.unapply(t, anon.typeTree.span) 
          do
            if spec.hasSpecializedParams then
              report.error(
                """
                Anonymous classes acting as instances of Specialized traits may not mix in other traits; 
                You can make a named object instead if you like.
                """, 
                anon.srcPos
              )
        }
        
        def checkAnonSpecializedBodyAndParents(spec: Specialization): Unit = {
          
          if !spec.hasSpecializedParams then return 
            
          
          // Only allowed to contain evidence parameters
          if !anon.body.forall(x => x.symbol.name.is(ContextBoundParamName)) then
            report.error(
              """
              Anonymous classes acting as instances of Specialized traits may not have additional members; 
              you can make a named object instead if you like.
              """,
              anon.srcPos
            )                    

          def hasOnlyValidParents: Boolean = anon.parentCalls match { 
            case (obj :: parentsOfSpecTrait) :+ (app@Apply(_, _)) =>
              val isFirstParentObject = obj.symbol.owner == ctx.definitions.ObjectClass
              val validOtherParents =
                parentsOfSpecTrait.forall(x => spec.symbol.asClass.baseClasses.contains(x.symbol.owner))                    
              isFirstParentObject && validOtherParents
            case _ => false
          }
                      
          if !hasOnlyValidParents then
            report.error(
              """
              Anonymous classes acting as instances of Specialized traits may not mix in other traits; 
              you can make a named object instead if you like.
              """,
              anon.srcPos
            )

          registerSpecializationsInUnit()
        }
        
        anon.typeTree.tpe match {
          case tpe: AndType => /* Multiple mixed in traits will be typed as an AndType */ 
            checkAnonSpecializedMixins(tpe)
          
          case tpe =>
            Specialization
              .unapply(tpe, anon.typeTree.span)
              .foreach(spec => checkAnonSpecializedBodyAndParents(spec))
        }
      case _ =>
    }

    private def checkSpecializationTypeArgs(specialization: Specialization)(using Context): Unit =
      if specialization.hasSpecializedParams then
        // Block Vec[?] and similar
        specialization.specializedTypeArgs.filter {
          case _: WildcardType => true
          case _: RealTypeBounds => true
          case _ => false
        }.foreach { _ =>
          report.error(
            "Wildcard types may not be substituted for Specialized type parameters.", 
            ctx.source.atSpan(specialization.span)
          )
        }

      if !specialization.isSpecialized then
        specialization.specializedTypeArgs.flatMap {
          // For each type we are using in a Specialized position
          // Find all type params within that type that are not marked as Specialized so we can error
          arg =>
            arg.widen.dealias.namedPartsWith { part =>
              def hasCorrectlyMarkedTypeParam: Boolean =
                val owner = 
                  if part.typeSymbol.owner.isClass then 
                    part.typeSymbol.owner.primaryConstructor 
                  else 
                    part.typeSymbol.owner

                owner.paramSymss.flatten.exists { sym => sym.info match
                  case SpecializedEvidence(tpeArg) =>
                    tpeArg.typeSymbol.isTypeParam && tpeArg.typeSymbol.name == part.name
                  case _ => false
                }

              part.typeSymbol.isTypeParam && !hasCorrectlyMarkedTypeParam
            }
        }.foreach { tpe =>
          if tpe.denot.symbol.srcPos.span.exists then
            report.error(
              s"${tpe.typeSymbol} used in a Specialized position, so it must be marked as Specialized at its definition.", 
              tpe.denot.symbol.srcPos
            )
        }

    private inline def registerSpecializationsInUnit()(using Context): Unit = { 
      ctx.compilationUnit.needsInlining = true
      ctx.compilationUnit.hasSpecializations = true
    }
