trait T { def foo: Int }
trait T1 extends T { override def foo = super.foo } // error: method foo in trait T is accessed from super.
trait T2 extends T { override def foo = super.foo } // error: method foo in trait T is accessed from super.
object Test extends T2 with T1 {
  def main(args: Array[String]) = {
    assert(foo == 3)
  }
}
