import scala.compiletime.ops.int.*

object Test {
  class Vec[S <: Int] {
    infix def concat [RS <: Int](that : Vec[RS]) : Vec[S + RS] = new Vec[S + RS]
  }

  val v1 = new Vec[1]
  val v2 = new Vec[2]
  val v3 : Vec[3] = v1 concat v2
  val v3a = v1 concat v2
  val v3b : Vec[3] = v3a
}