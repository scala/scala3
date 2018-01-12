case class Foo[T <: Int with Singleton](t : T)

object Test {
  val one = 1
  final val final_one = 1
  val a : 1 = Foo(1).t
  val b : Int = Foo(one).t // error: does not conform to upper bound Int & Singleton
  val c : 1 = Foo(final_one).t
}
