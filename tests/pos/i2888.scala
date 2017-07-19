trait Foo[A, CC[X] <: Foo[X, CC, CC[X]], C <: CC[A]] {

  def cc: CC[A]

  def foo: Unit = ()

  def bar: Unit = cc.foo

}

object Main {
  def main(args: Array[String]): Unit = ()
}
