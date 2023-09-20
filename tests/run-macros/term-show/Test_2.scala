//> using options -experimental

import scala.language.experimental.erasedDefinitions

trait A:
  def imp(x: Int)(implicit str: String): Int
  def use(x: Int)(using str: String): Int
  def era(x: Int)(erased str: String): Int

  def f1(x1: Int, erased x2: Int): Int
  def f2(erased x1: Int, erased x2: Int): Int
  def f3(using x1: Int, erased x2: Int): Int
  def f4(using erased x1: Int, erased x2: Int): Int

object Test {
  import TypeToolbox.*
  def main(args: Array[String]): Unit = {
   println(show {
      class C {
        def a = 0
        private def b = 0
        private[this] def c = 0
        private[C] def d = 0
        protected def e = 0
        protected[this] def f = 0
        protected[C] def g = 0
      }
    })

    println(showTree("A"))
  }
}
