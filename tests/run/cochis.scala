import Predef.{$conforms as _, *}

trait A {

  implicit def id[A] : A => A  =  x => x // (1)
  def trans[A] (x : A) (implicit f : A => A) = f(x)
}
object Test extends A with App {
  implicit def succ : Int ⇒ Int = x ⇒ x + 1 // (3)
  def bad [A] (x : A) : A = trans[A](x) // (4) incoherent denition !
  val v1 = bad [Int] (3) // (5) evaluates to 3
  val v2 = trans [Int] (3) // (6) substituting bad by trans is rejected
  assert(v1 == 3)
  assert(v2 == 4)
}
