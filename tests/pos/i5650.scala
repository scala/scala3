sealed abstract class N
trait Z extends N
trait S[P] extends N

object Test {
  type T[X] <: N =
    X match {
      case Z => Z
      case S[p] => T[p]
    }
}
