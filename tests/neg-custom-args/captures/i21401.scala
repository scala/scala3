import language.experimental.captureChecking

trait IO:
  def println(s: String): Unit
def usingIO[R](op: IO^ => R): R = ???

case class Boxed[+T](unbox: T)

type Res = [R, X <: Boxed[IO^] -> R] -> (op: X) -> R
def mkRes(x: IO^): Res =
  [R, X <: Boxed[IO^] -> R] => (op: X) =>
    val op1: Boxed[IO^] -> R = op
    op1(Boxed[IO^](x))
def test2() =
  val a = usingIO[IO^](x => x) // error
  val leaked: [R, X <: Boxed[IO^] -> R] -> (op: X) -> R = usingIO[Res](mkRes) // error
  val x: Boxed[IO^] = leaked[Boxed[IO^], Boxed[IO^] -> Boxed[IO^]](x => x) // error
  val y: IO^{x*} = x.unbox // was error
  y.println("boom")
