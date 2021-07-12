
import compiletime.*
import compiletime.ops.int.*

object Test extends App {

  inline def toInt[N]: Int =
    inline constValue[N] match {
      case _: S[n1] => 1 + toInt[n1]
      case 0 => 0
    }

  println(toInt[0])
  println(toInt[1])
  println(toInt[2])

  locally {
    inline def toInt[N]: Int =
      inline constValueOpt[N] match {
        case Some(_: S[n1]) => 1 + toInt[n1]
        case Some(0) => 0
        case None => 0
      }
    println(toInt[0])
    println(toInt[1])
    println(toInt[2])
  }

  val xs = List(1, 2, 3)

  inline def select(n: Int) =
    inline constValueOpt[n.type] match {
      case Some(0) => xs(0)
      case Some(1) => xs(1)
      case Some(2) => xs(2)
      case Some(_) => -1
    }

  println(select(0))
  println(select(1))
  println(select(2))
  println(select(3))
  final val idx = 0
  println(select(idx))
}



