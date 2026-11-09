trait Foo {
  val foo: Option[String] = ???
}
class Bar extends Foo

object Test:
  def main(args: Array[String]): Unit =
    classOf[Bar].getMethods.sortBy(_.getName).filter(_.getName.contains("foo")).foreach(m => {
      println(m)
      println(m.toGenericString)
    })
