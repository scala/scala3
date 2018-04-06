package streams

import dotty.tools.dotc.quoted.Toolbox._
import scala.quoted._

object Test {

  def test1() = StagedStreams.Stream.of('{Array(1,2,3)})
}