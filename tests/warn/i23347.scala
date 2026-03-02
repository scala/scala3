//> using options -Wunused:all

object USED {
  case class A(value: Int)
}

object UNUSED {
  // In reality UNUSED would contain several other necessary members!
  private type A = USED.A // warn private
  class B
}

object Test {
  import USED.*
  import UNUSED.*

  def foo(a: A): Int = a.value

  def g(b: B) = ()

}

