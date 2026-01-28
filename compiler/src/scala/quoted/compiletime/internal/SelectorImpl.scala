package scala.quoted.compiletime.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.StdNames.nme
import scala.annotation.experimental
import scala.quoted.compiletime as pub

/////// Selector ///////////////////////////////////////////////////////////////

type Selector = pub.Selector & SelectorImpl
sealed trait SelectorImpl { _self: pub.Selector =>
  val underlying: untpd.ImportSelector
}
object SelectorImpl {
  def apply(x: untpd.ImportSelector): SelectorImpl =
    if x.isGiven then new GivenSelectorImpl(x)
    else if x.renamed.isEmpty then new SimpleSelectorImpl(x)
    else x.renamed match
      case untpd.Ident(nme.WILDCARD) => new OmitSelectorImpl(x)
      case _ => new RenameSelectorImpl(x)

  object Module extends pub.Selector.Module {}
}

/////// SimpleSelector ///////////////////////////////////////////////////////////////

type SimpleSelector = SimpleSelectorImpl
final class SimpleSelectorImpl(val underlying: untpd.ImportSelector) extends SelectorImpl, pub.SimpleSelector {
  override def name: String = ???
  override def namePos: pub.Position = ???
}
object SimpleSelectorImpl {
  object Module extends pub.SimpleSelector.Module {
    @experimental override def apply(name: String): pub.SimpleSelector = ???
    @experimental override def make(name: String): pub.SimpleSelector = ???
  }
}

/////// RenameSelector ///////////////////////////////////////////////////////////////

type RenameSelector = RenameSelectorImpl
final class RenameSelectorImpl(val underlying: untpd.ImportSelector) extends SelectorImpl, pub.RenameSelector {
  override def fromName: String = ???
  override def fromPos: pub.Position = ???
  override def toName: String = ???
  override def toPos: pub.Position = ???
}
object RenameSelectorImpl {
  object Module extends pub.RenameSelector.Module {
    @experimental override def apply(fromName: String, toName: String): pub.RenameSelector = ???
    @experimental override def make(fromName: String, toName: String): pub.RenameSelector = ???
  }
}

/////// OmitSelector ///////////////////////////////////////////////////////////////

type OmitSelector = OmitSelectorImpl
final class OmitSelectorImpl(val underlying: untpd.ImportSelector) extends SelectorImpl, pub.OmitSelector {
  override def name: String = ???
  override def namePos: pub.Position = ???
}
object OmitSelectorImpl {
  object Module extends pub.OmitSelector.Module {
    @experimental override def apply(name: String): pub.OmitSelector = ???
    @experimental override def make(name: String): pub.OmitSelector = ???
  }
}

/////// GivenSelector ///////////////////////////////////////////////////////////////

type GivenSelector = GivenSelectorImpl
final class GivenSelectorImpl(val underlying: untpd.ImportSelector) extends SelectorImpl, pub.GivenSelector {
  override def bound: Option[pub.TypeTree] = ???
}
object GivenSelectorImpl {
  object Module extends pub.GivenSelector.Module {
    @experimental override def apply(bound: Option[pub.TypeTree]): pub.GivenSelector = ???
    @experimental override def make(bound: Option[pub.TypeTree]): pub.GivenSelector = ???
  }
}