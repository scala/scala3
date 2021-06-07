import language.implicitConversions

trait AdditiveSemigroup[A]

final class AdditiveSemigroupOps[A](lhs: A)(implicit as: AdditiveSemigroup[A]) {
  def +(rhs: A): A = ???
  def ^(rhs: A): A = ???
}

trait AdditiveSemigroupSyntax {
  implicit def additiveSemigroupOps[A: AdditiveSemigroup](a: A): AdditiveSemigroupOps[A] =
     new AdditiveSemigroupOps(a)
}

object syntax {
  object additiveSemigroup extends AdditiveSemigroupSyntax
}

object App {

  def main(args: Array[String]): Unit = {
    import syntax.additiveSemigroup._

    implicit def IntAlgebra[A]: AdditiveSemigroup[Map[Int, A]] = ???

    def res[A]: Map[Int, A] = {
      val a: Map[Int, A] = Map.empty
      val b: Map[Int, A] = Map.empty
      // Calls the operator on AdditiveSemigroupOps
      a ^ b
      // Calls the operator + on AdditiveSemigroupOps only in Scala 2
      // In Scala 3 tries to call `+` on Map
      a + b
    }
  }

}