case class Record(elems: (String, Any)*) extends Selectable {
  def selectDynamic(name: String): Any = elems.find(_._1 == name).get._2
}

object Test {
  import scala.reflect.Selectable.reflectiveSelectable

  def f(closeable: { def close(): Unit }) =
    closeable.close()

  type RN = Record { val name: String; val age: Int }

  def g(r: RN) = r.name

  val rr: RN = Record("name" -> "Bob", "age" -> 42).asInstanceOf[RN]

  def main(args: Array[String]): Unit = {
    f(new java.io.PrintStream("foo"))
    assert(g(rr) == "Bob")

    val s: { def concat(s: String): String } = "abc"
    assert(s.concat("def") == "abcdef")
  }
}

