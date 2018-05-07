import java.io.File

import scala.quoted._
object Macros {
  inline def foo(): Int = ~fooImpl()
  def fooImpl(): Expr[Int] = {
    (new File("dfsdafsd")).exists()
    '(1)
  }
}
