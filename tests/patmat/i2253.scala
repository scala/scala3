sealed trait S

object BodylessObject extends S

object HasIntM extends S {
  type M = Int
}

object HasStringXStringM extends S {
  type M = String
  val x: String = ""
}

object HasIntXStringM extends S {
  type M = String
  val x: Int = 0
}

object HasIntXIntM extends S {
  type M = Int
  val x: Int = 0
}

trait T
case class TA(val x: Int) extends T with S

class Test {
  def onlyIntX(s: S { val x: Int }) = s match { case _: T => ; }
  def exposeAlias1[I <: Int](s: S { type M = I; val x: Int }) = s match { case _: T => ; }
  def exposeAlias2[I <: Int](s: S { val x: Int; type M = I }) = s match { case _: T => ; }
}
