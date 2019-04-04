
// Check that the return type of toString() isn't nullable.
class Foo {

  val x: java.lang.Integer = 42
  val s: String = x.toString()

}
