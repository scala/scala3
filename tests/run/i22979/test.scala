// scalajs: --skip

import util.Try

@main def Test =
  assert(Try(classOf[Leak].getDeclaredField("l")).isFailure)
  assert(classOf[Leak].getFields.length == 0)
  //classOf[Leak].getFields.map(_.getName).foreach(println) //DEBUG
  assert(classOf[C].getFields.length == 0)
  //classOf[Lapse.type].getFields.map(_.getName).foreach(println) //DEBUG

class C:
  private val x = 42
  println(x)
  println(List(27).map(_ + x))

// The easy tweak for lambdas does not work for module owner.
// The lambdalifted anonfun is not transformed correctly.
class Lapse:
  def f = Lapse.DefaultSentinelFn()
object Lapse:
  private val DefaultSentinel: AnyRef = new AnyRef
  private val DefaultSentinelFn: () => AnyRef = () => DefaultSentinel
