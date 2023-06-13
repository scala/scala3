import scala.deriving._
import scala.annotation.experimental
import scala.quoted._

import MirrorOps.*

object Test extends App:

  case class WithDefault(x: Int, y: Int = 1)
  assert(overridesDefaultArgument[WithDefault])

  case class WithoutDefault(x: Int)
  assert(!overridesDefaultArgument[WithoutDefault])
