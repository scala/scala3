package dotty.tools.dotc
package printing

import core._
import Texts._, ast.Trees._
import Types.Type, Symbols.Symbol, Contexts.Context, Scopes.Scope, Constants.Constant,
       Names.Name, Denotations._, Annotations.Annotation
import typer.Implicits.SearchResult
import typer.ImportInfo

/** The base class of all printers
 */
abstract class Printer {

  private[this] var prec: Precedence = GlobalPrec

  /** The current precedence level */
  def currentPrecedence = prec

  /** Generate text using `op`, assuming a given precedence level `prec`. */
  def atPrec(prec: Precedence)(op: => Text): Text = {
    val outerPrec = this.prec
    this.prec = prec
    try op
    finally this.prec = outerPrec
  }

  /** Generate text using `op`, assuming a given precedence level `prec`.
   *  If new level `prec` is lower than previous level, put text in parentheses.
   */
  def changePrec(prec: Precedence)(op: => Text): Text =
    if (prec < this.prec) atPrec(prec) ("(" ~ op ~ ")") else atPrec(prec)(op)

  /** The name, possibley with with namespace suffix if debugNames is set:
   *  /L for local names, /V for other term names, /T for type names
   */
  def nameString(name: Name): String

  /** The name of the given symbol.
   *  If !settings.debug, the original name where
   *  expansions of operators are translated back to operator symbol.
   *  E.g. $eq => =.
   *  If settings.uniqid, adds id.
   */
  def nameString(sym: Symbol): String

  /** The fully qualified name of the symbol */
  def fullNameString(sym: Symbol): String

  /** The kind of the symbol */
  def kindString(sym: Symbol): String

  /** The name as a text */
  def toText(name: Name): Text

  /** Textual representation, including symbol's kind e.g., "class Foo", "method Bar".
   *  If hasMeaninglessName is true, uses the owner's name to disambiguate identity.
   */
  def toText(sym: Symbol): Text

  /** Textual representation of symbol's declaration */
  def dclText(sym: Symbol): Text

  /** Textual representation of single denotation's declaration */
  def dclText(sd: SingleDenotation): Text

  /** If symbol's owner is a printable class C, the text "in C", otherwise "" */
  def locationText(sym: Symbol): Text

  /** Textual representation of symbol and its location */
  def locatedText(sym: Symbol): Text

  /** A description of sym's location */
  def extendedLocationText(sym: Symbol): Text

  /** Textual representation of denotation */
  def toText(denot: Denotation): Text

  /** Textual representation of constant */
  def toText(const: Constant): Text

  /** Textual representation of annotation */
  def toText(annot: Annotation): Text

  /** Textual representation of type */
  def toText(tp: Type): Text

  /** Textual representation of all symbols in given list,
   *  using `dclText` for displaying each.
   */
  def dclsText(syms: List[Symbol], sep: String = "\n"): Text

  /** Textual representation of all definitions in a scope using `dclText` for each */
  def toText(sc: Scope): Text

  /** Textual representation of tree */
  def toText[T >: Untyped](tree: Tree[T]): Text

  /** Textual representation of implicit search result */
  def toText(result: SearchResult): Text

  /** Textual representation of info relating to an import clause */
  def toText(result: ImportInfo): Text

      /** Perform string or text-producing operation `op` so that only a
   *  summarized text with given recursion depth is shown
   */
  def summarized[T](depth: Int)(op: => T): T

  /** A plain printer without any embellishments */
  def plain: Printer
}

