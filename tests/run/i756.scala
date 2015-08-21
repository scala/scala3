trait T { def foo: Int = 3 }
trait T1 extends T { override def foo = super.foo }
trait T2 extends T { override def foo = super.foo }
object Test extends T2 with T1 {
  def main(args: Array[String]) = {
    assert(foo == 3)
  }
}
