sealed trait ZIO[-R, +E, +A]
object ZIO{
  def fail[E](error: E): ZIO[Any, E, Nothing] = ???
}

trait Endpoint[INPUT, ERROR_OUTPUT, OUTPUT]{
  sealed trait ZServerEndpoint[R]
  def zServerLogic[R](logic: INPUT => ZIO[R, ERROR_OUTPUT, OUTPUT]): ZServerEndpoint[R] = ???
}

@main def Test() =
  val x: Endpoint[?, Unit, Unit] = ???
  x.zServerLogic[Any](_ => ZIO.fail(new RuntimeException("boom")))