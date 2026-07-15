package dotty.tools.dotc
package qualified_types

import dotty.tools.dotc.ast.tpd.{MemberDef, Tree, TreeTraverser}
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.{AnnotatedType, Type}
import dotty.tools.dotc.report

/** Checks that [[ANF]] eliminated the skolem encoding entirely: no
 *  `ENodeVar.Skolem` remains inside any qualifier, and no
 *  `@QualifierSkolemIndex` annotation remains (on a type or a symbol).
 *
 *  Opt-in: runs right after [[ANF]], only under `-Yqualified-types-anf`.
 */
class ANFCheck extends Phase:

  override def phaseName: String = ANFCheck.name
  override def description: String = ANFCheck.description

  override def isRunnable(using Context): Boolean =
    super.isRunnable && ctx.settings.YqualifiedTypesAnf.value

  /** Appended to every warning: these residual cases are not yet handled (e.g.
   *  skolems in a method's own result qualifier, or in a function type), so we
   *  warn rather than fail, and ask for them to be reported.
   */
  private val note =
    "The qualifier skolem encoding could not be fully eliminated here; please report this."

  override def run(using Context): Unit =
    val checker = new TreeTraverser:
      def traverse(tree: Tree)(using Context): Unit =
        checkType(tree.tpe, tree)
        tree match
          case tree: MemberDef if tree.symbol.exists =>
            checkType(tree.symbol.info, tree)
            if tree.symbol.hasAnnotation(defn.QualifierSkolemIndexAnnot) then
              report.warning(em"ANF check: residual @QualifierSkolemIndex on ${tree.symbol}. $note", tree.srcPos)
          case _ => ()
        traverseChildren(tree)
    checker.traverse(ctx.compilationUnit.tpdTree)

  private def checkType(tp: Type, tree: Tree)(using Context): Unit =
    tp.foreachPart:
      case AnnotatedType(_, annot) if annot.symbol == defn.QualifierSkolemIndexAnnot =>
        report.warning(em"ANF check: residual @QualifierSkolemIndex in type $tp. $note", tree.srcPos)
      case QualifiedType(_, qualifier) =>
        qualifier.foreachType:
          case sk: ENodeVar.Skolem =>
            report.warning(em"ANF check: residual skolem $sk in qualifier of $tp. $note", tree.srcPos)
          case _ => ()
      case _ => ()

object ANFCheck:
  val name: String = "qualifiedTypesANFCheck"
  val description: String = "check that the qualifier skolem encoding was eliminated"
