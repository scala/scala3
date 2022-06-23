// scalajs: --skip --pending

import scala.annotation.static

class Base

object Base {
  @static val x = 10
  @static final val y = 10
  @static var a = 10
  @static final var b = 10
  @static def f: Int = 30
}

object Test {
  def main(args: Array[String]): Unit = Base
}