// scalajs: --skip

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

class A() {
  val bc = B.C(50)
}

object A {
  Thread.sleep(100L)

  val a = new A()
}


object B {
  Thread.sleep(100L)

  case class C(a: Int)

  val a: A = A.a
}

object Test {
  def main(args: Array[String]): Unit = {
    Await.result(Future.sequence(Seq(
      Future { A.a },
      Future { B.a },
    )), 1.seconds)
  }
}
