package dotty.tools.pc

import scala.meta.pc.OffsetParams

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.PcSymbolSearch.*
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j as l

final class PcRenameProvider(
    driver: InteractiveDriver,
    params: OffsetParams,
    name: Option[String]
) extends WithSymbolSearchCollector[l.TextEdit](driver, params):
  private val forbiddenMethods =
    Set("equals", "hashCode", "unapply", "apply", "<init>", "unary_!", "!")

  private val soughtSymbolNames = soughtSymbols match
    case Some((symbols, _)) =>
      symbols.filterNot(_.isError).map(symbol => symbol.decodedName.toString)
    case None => Set.empty[String]

  def canRenameSymbol(sym: Symbol)(using Context): Boolean =
    val decodedName = sym.decodedName
    def isForbiddenMethod = sym.is(Method) && forbiddenMethods(decodedName)
    def local = sym.ownersIterator.drop(1).exists(ow => ow.is(Method)) || isLocalNamedTupleField(sym)
    def isInWorksheet = sym.source.path.isWorksheet
    !isForbiddenMethod && (local || isInWorksheet) && soughtSymbolNames(decodedName)

  /** A named tuple field is declared by type literals, which other files may use too.
   *  It can be renamed when its tuple type is spelled out in this file, does not come in
   *  from another file, and does not escape through the signature, explicit or inferred,
   *  of a definition visible outside. Structurally equal types of other files are not seen.
   */
  private def isLocalNamedTupleField(sym: Symbol)(using Context): Boolean =
    val fieldNames = namedTupleFieldsOf(sym.info).map(_._1)
    def isFieldTuple(tpe: Type) = tpe.exists && namedTupleFieldsOf(tpe).map(_._1) == fieldNames
    def hasFieldTuple(tpe: Type) = tpe.exists && tpe.existsPart(isFieldTuple, forceLazy = false)
    def isLocal(sym: Symbol) =
      val definition = if sym.is(Param) then sym.owner else sym
      definition.ownersIterator.drop(1).exists(_.is(Method))
    def definedHere(sym: Symbol) = sym.exists && sym.source == unit.source

    abstract class Finder extends TreeAccumulator[Boolean]:
      def find(tree: Tree)(using Context): Boolean
      def apply(found: Boolean, tree: Tree)(using Context): Boolean = found || (tree match
        case tree: Inlined => this(this(false, tree.call), tree.bindings) // inlined code of other files
        case _ => find(tree))

    /** Is the tuple type spelled out here, as a type or a constructor? */
    val declares = new Finder:
      def find(tree: Tree)(using Context) = tree match
        case NamedTupleTree(names, values) if isFieldTuple(defn.NamedTuple(names.tpe, values.tpe)) => true
        case _ => foldOver(false, tree)

    /** Does the tuple type come from another file, as the type of a definition there or as
     *  a type that is not spelled out, like `NamedTuple.From[P]`?
     */
    val imported = new Finder:
      def find(tree: Tree)(using Context) = tree match
        case NamedTupleTree(_, _) => foldOver(false, tree)
        case tree: RefTree =>
          !definedHere(tree.symbol) && (if tree.isType then isFieldTuple(tree.tpe) else hasFieldTuple(tree.symbol.info))
          || foldOver(false, tree)
        case tree if tree.isType && !tree.isInstanceOf[TypeTree] && isFieldTuple(tree.tpe) => true
        case _ => foldOver(false, tree)

    /** Does the tuple type escape through the signature of a definition visible outside this file? */
    val escapes = new Finder:
      def find(tree: Tree)(using Context) = tree match
        case tree: ValOrDefDef if !isLocal(tree.symbol) =>
          val inferredParam = tree.symbol.is(Param) && tree.tpt.isInstanceOf[TypeTree] // never escapes
          !inferredParam && hasFieldTuple(tree.tpt.tpe) || foldOver(false, tree)
        case tree: TypeDef if !isLocal(tree.symbol) =>
          val signature = tree.rhs match
            case impl: Template => impl.parents.map(_.tpe) ++ Option.unless(impl.self.isEmpty)(impl.self.tpt.tpe)
            case rhs => List(rhs.tpe)
          signature.exists(hasFieldTuple) || foldOver(false, tree)
        case _ => foldOver(false, tree)

    isNamedTupleField(sym) && fieldNames.nonEmpty
    && declares(false, unit.tpdTree) && !imported(false, unit.tpdTree)
    && (unit.source.path.isWorksheet || !escapes(false, unit.tpdTree))

  def prepareRename(): Option[l.Range] =
    soughtSymbols.flatMap((symbols, pos) =>
      if symbols.forall(canRenameSymbol) then Some(pos.toLsp)
      else None
    )

  val newName = name.map(_.stripBackticks.backticked).getOrElse("newName")

  def collect(
      parent: Option[Tree]
  )(tree: Tree | EndMarker, toAdjust: SourcePosition, sym: Option[Symbol]): l.TextEdit =
    val (pos, stripBackticks) = toAdjust.adjust(text, forRename = true)
    // a named tuple field declared as an explicit literal type `NamedTuple[("name"), ...]`
    val inLiteral = pos.start > 0 && text(pos.start - 1) == '"'
    l.TextEdit(
      pos.toLsp,
      if stripBackticks || inLiteral then newName.stripBackticks else newName
    )

  def rename(): List[l.TextEdit] =
    val (symbols, _) = soughtSymbols.getOrElse(Set.empty, pos)
    if symbols.nonEmpty && symbols.forall(canRenameSymbol(_))
    then result()
    else Nil
end PcRenameProvider
