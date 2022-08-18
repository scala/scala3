object Test extends App {

  val one = 1
  val two = "two"
  val threeHalf = 3.5

  // Test escaping
  println(raw"$one plus $two\nis $threeHalf")
  println(s"$one plus $two\nis $threeHalf")

  // Test empty strings between elements
  println(raw"a$one$two${threeHalf}b")
  println(s"a$one$two${threeHalf}b")

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
