import scala.annotation.tailrec
import scala.util.chaining.given

object Test {
  class Ctx
  type Op[A] = Ctx ?=> A

  var min = Int.MaxValue
  var max = 0
  def stk = new Throwable().getStackTrace.length

  @tailrec def f[A](n: Int)(op: Op[A]): A =
    val depth = stk
    min = min.min(depth)
    max = max.max(depth)
    given Ctx = Ctx()
    if (n > 0) f(n-1)(op)
    else op

  def g(ctx: Ctx) = stk

  def main(args: Array[String]): Unit =
    val extra = 3
    f(10)(Ctx ?=> g(summon[Ctx])).tap(res => assert(res <= max + extra, s"min $min, max $max, ran g at $res"))
}