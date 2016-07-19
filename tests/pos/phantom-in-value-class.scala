
object PhantomInValueClass {
  import BooUtil._
  new VC("ghi").foo(boo)
}

object BooUtil extends Phantom {

  type Boo <: this.Any
  def boo: Boo = assume

  class VC[T](val x: T) extends AnyVal {
    def foo(b: Boo) = println(x)
  }

}
