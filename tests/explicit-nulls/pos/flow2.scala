
class Foo {

  val x: String|Null = ???
  val y: String|Null = ???
  val z: String|Null = ???

  if ((x != null && z != null) || (y != null && z != null)) {
    val z2: String = z
  }
}
