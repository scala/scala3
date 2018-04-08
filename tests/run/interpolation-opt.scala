object Test extends App {

  val one = 1
  val two = "two"
  val three = 3.0

  // Test escaping
  println(raw"$one plus $two\nis $three")
  println(s"$one plus $two\nis $three")

  // Test empty strings between elements
  println(raw"a$one$two${three}b")
  println(s"a$one$two${three}b")

  // Test empty string interpolators
  println(raw"")
  println(s"")

  // Make sure that StringContext still works with idents
  val foo = "Hello"
  val bar = "World"
  println(StringContext(foo, bar).s(" "))

  def myStringContext= { println("Side effect!"); StringContext }
  println(myStringContext("Foo", "Bar").s(" ")) // this shouldn't be optimised away

  // this shouldn't be optimised away
  println({ println("Side effect n2!"); StringContext }.apply("Titi", "Toto").s(" "))
}
