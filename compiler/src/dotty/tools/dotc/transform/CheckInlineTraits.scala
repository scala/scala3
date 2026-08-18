package dotty.tools.dotc.transform

import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.IllegalUseOfSpecialized
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.NameKinds.ContextBoundParamName
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.transform.Specialization.isSpecializationCandidate
import dotty.tools.dotc.transform.Specialization.isSpecializedTrait
import dotty.tools.dotc.core.Flags

object CheckInlineTraits:
  val name: String = "checkInlineTraits"
  val description: String = "check if inline & specialized traits are present and are correctly used"

class CheckInlineTraits extends MiniPhase:
    override def phaseName: String = CheckInlineTraits.name 

    override def description: String = CheckInlineTraits.description

    override def changesMembers: Boolean = false
    
    override def changesParents: Boolean = false

    override def runsAfter: Set[String] = Set("typer")

    override def transformIdent(tree: Ident)(using Context): Tree = {
      val sym = tree.symbol
      if !sym.isType then 
        if sym == defn.SpecializedModule && (ctx.owner ne defn.SpecializedModule.moduleClass) then
          report.error(IllegalUseOfSpecialized(), tree.srcPos)
        
        if sym == defn.SpecializedModule_apply then 
          registerSpecializationsInUnit

      tree
    }

    private def checkInlTraitPrivateMemberIsLocal(tree: ValOrDefDef)(using Context): Unit =
      val sym = tree.symbol
      if sym.exists && !sym.is(Synthetic) && sym.owner.isInlineTrait && sym.isAllOf(Private, butNot = Local) then
        report.error(
          em"""
            implementation restriction: inline traits cannot have non-local private members. 
            This also means no retained inline methods.
          """, 
          tree.srcPos
        )
  
          
    override def transformValDef(tree: ValDef)(using Context): Tree = {
      checkInlTraitPrivateMemberIsLocal(tree)

      tree
    }

    override def transformDefDef(tree: DefDef)(using Context): Tree = {
      checkInlTraitPrivateMemberIsLocal(tree)

      val sym = tree.symbol 
      val isConstructorOfNonInlineType = sym.isConstructor && !sym.owner.is(Inline)
      val isRegularDefDef = !sym.isConstructor && !sym.is(Inline) 
      if isConstructorOfNonInlineType || isRegularDefDef then
        tree.paramss.flatten.foreach {
          param => if SpecializedEvidence.unapply(param.tpe.widen.dealias).nonEmpty then 
            report.error(s"Only inline traits and inline functions may take Specialized type parameters", param.srcPos)
        }

      tree
    }

    override def prepareForOther(tree: Tree)(using Context): Context = tree match {
      case tree: AppliedTypeTree =>
        val sym = tree.tpt.symbol
        if sym != defn.orType && sym != defn.andType && sym == defn.SpecializedClass && 
            !(ctx.owner.name.is(ContextBoundParamName) || ctx.owner.ownersIterator.contains(defn.SpecializedModule_apply)) then
          report.error(IllegalUseOfSpecialized(), tree.srcPos)
        ctx

      case _ => ctx
    }

    override def transformTypeDef(tree: TypeDef)(using Context): Tree = {
      val containUseOfSpecialized = 
        tree.rhs.tpe.existsPart(t => t.typeSymbol == defn.SpecializedClass.asType) 
      if containUseOfSpecialized && (tree.symbol ne defn.SpecializedClass) then
        report.error(IllegalUseOfSpecialized(), tree.srcPos)
        
      if tree.symbol.isInlineTrait then 
        registerSpecializationsInUnit

      tree
    }

    override def transformBlock(tree: Block)(using Context): Tree = tree match {
      case AnonymousClassInstance(anon) =>
        def deandify(tp: Type): Iterator[Type] = tp match {
          case AndType(l, r) => deandify(l) ++ deandify(r)
          case _ => Iterator.single(tp)
        }

        def checkSpecializedMixins(tpe: AndType) = deandify(tpe) foreach {
          Specialization.unapply(_, anon.typeTree.span) foreach {
            spec => if spec.hasSpecializedParams then
              report.error(
                """
                Anonymous classes acting as instances of Specialized traits may not mix in other traits; 
                You can make a named object instead if you like.
                """, 
                anon.srcPos
              )
          }
        }
        anon.typeTree.tpe match {
          case tpe: AndType => /* Multiple mixed in traits will be typed as an AndType */ 
            checkSpecializedMixins(tpe)

            tree 
          
          case tpe =>
            Specialization
              .unapply(tpe, anon.typeTree.span)
              .foreach(spec => 
                if spec.hasSpecializedParams then
                  // Only allowed to contain evidence parameters
                  if anon.body.filterNot(x => x.symbol.name.is(ContextBoundParamName)).nonEmpty then 
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
                        parentsOfSpecTrait.forall(x => spec.symbol.asClass.baseClasses.exists(p => p == x.symbol.owner))
                      
                      isFirstParentObject && validOtherParents
                    case _ => false
                  }
                      
                  if !hasOnlyValidParents then
                    report.error(
                      """
                      Anonymous classes acting as instances of Specialized traits may not mix in other traits; 
                      you can make a named object instead if you like.""", 
                      anon.srcPos
                    )

                  registerSpecializationsInUnit
                )
              
            tree
        }
      case _ => tree
    }

    override def transformTemplate(tree: Template)(using Context): Tree = {
      if tree.parents.exists(parent => Inlines.symbolFromParent(parent).isInlineTrait) then 
        registerSpecializationsInUnit

      tree
    }

    private inline def registerSpecializationsInUnit(using Context): Unit = { 
      ctx.compilationUnit.needsInlining = true
      ctx.compilationUnit.hasSpecializations = true
    }
