package dotty.tools
package dotc

import core._
import Contexts._
import SymDenotations.{ClassDenotation, NoDenotation}
import Symbols._
import util.{FreshNameCreator, SourceFile, NoSource}
import util.Spans.Span
import ast.{tpd, untpd}
import tpd.{Tree, TreeTraverser}
import typer.Nullables
import transform.SymUtils._
import core.Decorators._
import config.SourceVersion
import scala.annotation.internal.sharable

class CompilationUnit protected (val source: SourceFile) {

  override def toString: String = source.toString

  var untpdTree: untpd.Tree = untpd.EmptyTree

  var tpdTree: tpd.Tree = tpd.EmptyTree

  /** Is this the compilation unit of a Java file */
  def isJava: Boolean = source.file.name.endsWith(".java")

  /** The source version for this unit, as determined by a language import */
  var sourceVersion: Option[SourceVersion] = None

  /** Pickled TASTY binaries, indexed by class. */
  var pickled: Map[ClassSymbol, () => Array[Byte]] = Map()

  /** The fresh name creator for the current unit.
   *  FIXME(#7661): This is not fine-grained enough to enable reproducible builds,
   *  see https://github.com/scala/scala/commit/f50ec3c866263448d803139e119b33afb04ec2bc
   */
  val freshNames: FreshNameCreator = new FreshNameCreator.Default

  /** Will be set to `true` if there are inline call that must be inlined after typer.
   *  The information is used in phase `Inlining` in order to avoid traversing trees that need no transformations.
   */
  var needsInlining: Boolean = false

  var hasMacroAnnotations: Boolean = false

  /** Set to `true` if inliner added anonymous mirrors that need to be completed */
  var needsMirrorSupport: Boolean = false

  /** Will be set to `true` if contains `Quote`.
   *  The information is used in phase `Staging`/`Splicing`/`PickleQuotes` in order to avoid traversing trees that need no transformations.
   */
  var needsStaging: Boolean = false

  var suspended: Boolean = false
  var suspendedAtInliningPhase: Boolean = false

  /** Can this compilation unit be suspended */
  def isSuspendable: Boolean = true

  /** Suspends the compilation unit by thowing a SuspendException
   *  and recording the suspended compilation unit
   */
  def suspend()(using Context): Nothing =
    assert(isSuspendable)
    if !suspended then
      if (ctx.settings.XprintSuspension.value)
        report.echo(i"suspended: $this")
      suspended = true
      ctx.run.nn.suspendedUnits += this
      if ctx.phase == Phases.inliningPhase then
        suspendedAtInliningPhase = true
    throw CompilationUnit.SuspendException()

  private var myAssignmentSpans: Map[Int, List[Span]] | Null = null

  /** A map from (name-) offsets of all local variables in this compilation unit
   *  that can be tracked for being not null to the list of spans of assignments
   *  to these variables.
   */
  def assignmentSpans(using Context): Map[Int, List[Span]] =
    if myAssignmentSpans == null then myAssignmentSpans = Nullables.assignmentSpans
    myAssignmentSpans.nn
}

@sharable object NoCompilationUnit extends CompilationUnit(NoSource) {

  override def isJava: Boolean = false

  override def suspend()(using Context): Nothing =
    throw CompilationUnit.SuspendException()

  override def assignmentSpans(using Context): Map[Int, List[Span]] = Map.empty
}

object CompilationUnit {

  class SuspendException extends Exception

  /** Make a compilation unit for top class `clsd` with the contents of the `unpickled` tree */
  def apply(clsd: ClassDenotation, unpickled: Tree, forceTrees: Boolean)(using Context): CompilationUnit =
    val file = clsd.symbol.associatedFile.nn
    apply(SourceFile(file, Array.empty[Char]), unpickled, forceTrees)

  /** Make a compilation unit, given picked bytes and unpickled tree */
  def apply(source: SourceFile, unpickled: Tree, forceTrees: Boolean)(using Context): CompilationUnit = {
    assert(!unpickled.isEmpty, unpickled)
    val unit1 = new CompilationUnit(source)
    unit1.tpdTree = unpickled
    if (forceTrees) {
      val force = new Force
      force.traverse(unit1.tpdTree)
      unit1.needsStaging = force.containsQuote
      unit1.needsInlining = force.containsInline
      unit1.hasMacroAnnotations = force.containsMacroAnnotation
    }
    unit1
  }

  /** Create a compilation unit corresponding to `source`.
   *  If `mustExist` is true, this will fail if `source` does not exist.
   */
  def apply(source: SourceFile, mustExist: Boolean = true)(using Context): CompilationUnit = {
    val src =
      if (!mustExist)
        source
      else if (source.file.isDirectory) {
        report.error(s"expected file, received directory '${source.file.path}'")
        NoSource
      }
      else if (!source.file.exists) {
        report.error(s"source file not found: ${source.file.path}")
        NoSource
      }
      else source
    new CompilationUnit(src)
  }

  /** Force the tree to be loaded */
  private class Force extends TreeTraverser {
    var containsQuote = false
    var containsInline = false
    var containsMacroAnnotation = false
    def traverse(tree: Tree)(using Context): Unit = {
      if (tree.symbol.isQuote)
        containsQuote = true
      if tree.symbol.is(Flags.Inline) then
        containsInline = true
      for annot <- tree.symbol.annotations do
        if annot.tree.symbol.denot != NoDenotation && annot.tree.symbol.owner.derivesFrom(defn.QuotedMacroAnnotationClass) then
          ctx.compilationUnit.hasMacroAnnotations = true
      traverseChildren(tree)
    }
  }
}
