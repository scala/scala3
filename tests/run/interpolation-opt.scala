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
}
