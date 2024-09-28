//> using options -source future
trait Ord[T]:
  def compare(x: T, y: T): Boolean

class C[T]

trait T:
  given intC: C[Int] // warn
  given intC2: C[Int] () // OK

  given [T]: Ord[T] with  // warn // warn
    def compare(x: T, y: T): Boolean = ???

  given [T](using Ord[T]): Ord[List[T]] with // warn // warn
    def compare(x: List[T], y: List[T]): Boolean = ???

  def f[T: Ord : C]() = ???  // warn

