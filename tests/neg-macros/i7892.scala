import scala.quoted._

package x {
  class CExprResult1[T]
}

def run(using s: Scope): Unit = {
  val cpsLeft: x.CExprResult1[?] = ???
  run1(cpsLeft) // error
}

def run1[L](using s: Scope)(cpsLeft: x.CExprResult1[L])(using s.Type[L]): Unit = ???
