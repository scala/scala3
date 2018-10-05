final class Foo(x: Int) {
  var title: String = _

  def get(msg: String): () => String = () => {
    title = "Mr."
    title + " Jack" + ", " + msg
  }

  val f = get("hello")

  if (x > 10)
    f()

  println(title)                 // error
}

final class Foo2(x: Int) {
  var title: String = _

  def get(msg: String): () => String = () => {
    title = "Mr."
    title + " Jack" + ", " + msg
  }

  val f = get("hello")

  if (x > 10)
    f()
  else
    f()

  println(title)
}