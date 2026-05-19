trait Foo {
  val foo: Option[String] = ???
}
object bar extends Foo

object Test:
  def main(args: Array[String]): Unit =
    println("Foo:")
    classOf[Foo].getMethods.sortBy(_.getName).filter(_.getName.contains("foo")).foreach(m => {
      println(m)
      println(m.toGenericString)
    })
    println("bar:")
    classOf[bar.type].getMethods.sortBy(_.getName).filter(_.getName.contains("foo")).foreach(m => {
      println(m)
      println(m.toGenericString)
    })

