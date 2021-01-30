import scala.quoted.*

package x {
  class CExprResult1[T]
}

def run(using Quotes) = {
  val cpsLeft: x.CExprResult1[?] = ???
  run1(cpsLeft) // error
}

def run1[L:Type](cpsLeft: x.CExprResult1[L]): Unit = ???
