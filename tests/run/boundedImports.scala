class C[X]

object A extends C[String] {

  val a: A.type = this
  val b: String = ""
  val c: Integer = 10

}

enum Color {
  case Red, Green, Blue

  def identity(x: Any): Any = ""
}

object Test extends App {
  val c = 0

  import A.{_: C[_] | String}
  assert(a eq A)
  assert(b == "")
  assert(c == 0)

  import Color.{_: Color}
  assert(identity("hello") == "hello")
}