case class Foo[T <: Int with Singleton](t : T)

object Test {
  val one = 1
  final val final_one = 1
  val a : 1 = Foo(1).t
  val b : one.type = Foo(one).t
  val c : 1 = Foo(final_one).t
}
