trait Foo {
  type Repr <: String
  def f2(x: Repr) = x.length
}
trait Fooz[Repr <: Array[?]] {
  def f0(x: Repr) = x.length
}

trait Bar[Repr <: List[?]] extends Foo with Fooz[Array[Int]] {
  def f1(x: Repr) = x.length
}
