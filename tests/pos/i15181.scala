object prelude:
  //def scombine[A](x: A, y: A): Semigroup[A] ?=> A = Semigroup.scombine(x,y)
  export Semigroup.*

trait Semigroup[A]:
  def scombine(x: A, y: A): A

object Semigroup:
  def scombine[A](x: A, y: A) = (s: Semigroup[A]) ?=> s.scombine(x,y)