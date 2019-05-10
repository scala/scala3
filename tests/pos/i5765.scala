import scala.language.higherKinds

trait LogDSL[L]

object LogDSL {
  implicit object LogRecsLogDSL extends LogDSL[String]
  def runLog(ls: String): String = ???
}

trait Direction[D] {
  def north: D
}

object Direction {
  // needs both instances to trigger the bug
  implicit def logDirection[L](implicit L: LogDSL[L]): Direction[L] = ???
  implicit def RotateDirection[D](implicit D: Direction[D]): Direction[Rotate.Rotation[D]] = ???
}

trait Rotate[R] {
  def rotate(r: R): R
}

object Rotate {
  implicit def logRotate[L](implicit L: LogDSL[L]): Rotate[L] = ???

  opaque type Rotation[T] = Int => T
}

object Main {
  // the instances have to be acquired through implicit resolution to cause the crash
  def north[D](implicit D: Direction[D]): D = ???
  def rotate[R](r: R)(implicit RR: Rotate[R]): R = ???

  def main(args: Array[String]): Unit = {
    // commenting out either the first or the second of these lines removes the compiler crash
    // removing the wrapping println call abolishes the crash
    println(LogDSL.runLog(rotate(north)))
    println(LogDSL.runLog(rotate(north)))
  }
}