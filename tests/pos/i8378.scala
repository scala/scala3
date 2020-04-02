trait Has[A]

trait A
trait B
trait C

trait ZLayer[-RIn, +E, +ROut]

object ZLayer {
  def fromServices[A0, A1, B](f: (A0, A1) => B): ZLayer[Has[A0] with Has[A1], Nothing, Has[B]] =
    ???
}

val live: ZLayer[Has[A] & Has[B], Nothing, Has[C]] =
  ZLayer.fromServices { (a: A, b: B) =>
    new C {}
  }
