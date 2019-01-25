def hello(name: String) = s"hello, $name"

val her = {
  println("...")
  "Eve"
}

type Labelled[T] = (String, T)

final def showLabelled[T](x: Labelled[T]) = s"${x(0)} : ${x(1)}"

object O {
  def hi = hello("Bob")
}
