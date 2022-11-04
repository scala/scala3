// scalajs: --skip --pending

import scala.annotation.static

trait Base

object Base {
  @static val x = 10
  @static final val y = 10
  @static def f: Int = 30
}

object Test {
  def main(args: Array[String]): Unit = Base
}