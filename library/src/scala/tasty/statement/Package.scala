package scala.tasty.statement

import scala.tasty.Extractor
import scala.tasty.term.Term

trait Package extends TopLevelStatement

object Package {
  type Data = (Term, List[TopLevelStatement])
  def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyPackage(arg)
}
