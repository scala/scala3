object Test extends App {

  val one = 1
  val two = "two"
  val threeHalf = 3.5

  def bracketed(s: String): String = "[" + s + "]"

  // Test escaping
  println(raw"$one plus $two\nis $threeHalf")
  println(s"$one plus $two\nis $threeHalf")

  // Test empty parts between interpolated elements
  println(raw"a$one$two${threeHalf}b")
  println(s"a$one$two${threeHalf}b")

  // Test empty string interpolators
  println(bracketed(raw""))
  println(bracketed(s""))

  // Make sure that StringContext still works with idents
  val foo = "Hello"
  val bar = "World"
  println(StringContext(foo, bar).s(" "))

  def myStringContext= { println("Side effect!"); StringContext }
  println(myStringContext("Foo", "Bar").s(" ")) // this shouldn't be optimised away

  // this shouldn't be optimised away
  println({ println("Side effect n2!"); StringContext }.apply("Titi", "Toto").s(" "))

  println(bracketed(f"${"hello"}%-10s")) // preserve trailing space in formatted output for constant element
}
