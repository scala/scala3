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
    /*
    On the other hand, this fails:
    
    Await.result(Future.sequence(Seq(
      Future { A.a },
      Future { B.a },
    ))(
      scala.collection.BuildFrom.buildFromIterableOps/*[Seq, Future[A], A]*/, global
    )
    , 1.seconds)

	the problem here is that there is not enough info to instantiate the type parameters of
	scala.collection.BuildFrom.buildFromIterableOps. If the BuildFrom is searched as an implicit,
	enough type variables are instantiated to correctly determine the parameters. But if the
	`buildFromIterableOps` is given explicitly, the problem is underconstrained and the type
	parameters are instantiated to `Nothing`.

    */
    
  }
}
