package scala.quoted.compiletime

import scala.annotation.experimental

/////// Selector ///////////////////////////////////////////////////////////////

/** Import/Export selectors:
 *  - SimpleSelector: `.bar` in `import foo.bar`
 *  - RenameSelector: `.{bar => baz}` in `export foo.{bar => baz}`
 *  - OmitSelector: `.{bar => _}` in `import foo.{bar => _}`
 *  - GivenSelector: `.given`/`.{given T}` in `export foo.given`/`import foo.{given T}`
 */
sealed trait Selector private[compiletime] ()
object Selector {

  def quoted(using quotes: Quotes): Selector.Module = quotes.reflectV2.Selector
  given moduleConversion: (quotes: Quotes) => Conversion[Selector.type, Selector.Module] = _ => quotes.reflectV2.Selector

  trait Module private[compiletime] () {}

}

/////// SimpleSelector ///////////////////////////////////////////////////////////////

/** Simple import/export selector: `.bar` in `import foo.bar`. */
trait SimpleSelector private[compiletime] () extends Selector {
  def name: String
  def namePos: Position
}
object SimpleSelector {

  def quoted(using quotes: Quotes): SimpleSelector.Module = quotes.reflectV2.SimpleSelector
  given moduleConversion: (quotes: Quotes) => Conversion[SimpleSelector.type, SimpleSelector.Module] = _ => quotes.reflectV2.SimpleSelector

  def unapply(x: SimpleSelector): Some[String] = Some(x.name)

  trait Module private[compiletime] () {
    @experimental def apply(name: String): SimpleSelector
    @experimental def make(name: String): SimpleSelector
  }

}

/////// RenameSelector ///////////////////////////////////////////////////////////////

/** Rename import/export selector: `.{bar => baz}` in `import foo.{bar => baz}`. */
trait RenameSelector private[compiletime] () extends Selector {
  def fromName: String
  def fromPos: Position
  def toName: String
  def toPos: Position
}
object RenameSelector {

  def quoted(using quotes: Quotes): RenameSelector.Module = quotes.reflectV2.RenameSelector
  given moduleConversion: (quotes: Quotes) => Conversion[RenameSelector.type, RenameSelector.Module] = _ => quotes.reflectV2.RenameSelector

  def unapply(x: RenameSelector): (String, String) = (x.fromName, x.toName)

  trait Module private[compiletime] () {
    @experimental def apply(fromName: String, toName: String): RenameSelector
    @experimental def make(fromName: String, toName: String): RenameSelector
  }

}

/////// OmitSelector ///////////////////////////////////////////////////////////////

/** Omit import/export selector: `.{bar => _}` in `import foo.{bar => _}`. */
trait OmitSelector private[compiletime] () extends Selector {
  def name: String
  def namePos: Position
}
object OmitSelector {

  def quoted(using quotes: Quotes): OmitSelector.Module = quotes.reflectV2.OmitSelector
  given moduleConversion: (quotes: Quotes) => Conversion[OmitSelector.type, OmitSelector.Module] = _ => quotes.reflectV2.OmitSelector

  def unapply(x: OmitSelector): Some[String] = Some(x.name)

  trait Module private[compiletime] () {
    @experimental def apply(name: String): OmitSelector
    @experimental def make(name: String): OmitSelector
  }

}

/////// GivenSelector ///////////////////////////////////////////////////////////////

/** given import/export selector: `.given`/`.{given T}` in `import foo.given`/`export foo.{given T}`. */
trait GivenSelector private[compiletime] () extends Selector {
  def bound: Option[TypeTree]
}
object GivenSelector {

  def quoted(using quotes: Quotes): GivenSelector.Module = quotes.reflectV2.GivenSelector
  given moduleConversion: (quotes: Quotes) => Conversion[GivenSelector.type, GivenSelector.Module] = _ => quotes.reflectV2.GivenSelector

  def unapply(x: GivenSelector): Some[Option[TypeTree]] = Some(x.bound)

  trait Module private[compiletime] () {
    @experimental def apply(bound: Option[TypeTree]): GivenSelector
    @experimental def make(bound: Option[TypeTree]): GivenSelector
  }

}
