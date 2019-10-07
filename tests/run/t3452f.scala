import language.higherKinds

trait GenSet[A]

trait GenSetTemplate[A, +CC[X] <: GenSet[X]] {
  def empty: CC[A] = ???
}

trait SetLike[A, +Self <: SetLike[A, Self] with Set[A]] {
  def empty: Self
}

abstract class Set[A] extends GenSet[A] with SetLike[A,Set[A]] with GenSetTemplate[A,Set]

object Test {
  def main(args: Array[String]): Unit = {
    locally(classOf[Set[_]]) // trigger classloading to verify class
  }
}
