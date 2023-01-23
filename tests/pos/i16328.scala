import scala.reflect.ClassTag

object Test {
  def getParamType[T: ClassTag](x: T => Int): T = ???

  def id[S](x: S): S = x

  def main(args: Array[String]) = {
    // worked before
    val a1 = getParamType((x: Int) => x)
    val a2: Int = a1 // ensure that we actually got a ClassTag for the right type

    // broken before
    val b1 = id(getParamType((x: Int) => x)) // was error
    val b2: Int = b1
    val c1 = id(id(getParamType((x: Int) => x))) // was error
    val c2: Int = c1
  }
}
