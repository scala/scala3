case class Record(elems: (String, Any)*)

object Record {

  implicit def projector: Projector[Record] = new Projector[Record] {
    def get(receiver: Record, name: String): Any =
      receiver.elems.find(_._1 == name).get._2
  }

}

object Test {
  import scala.reflect.Projector.reflectiveProjector
  import Record.projector

  def f(closeable: { def close(): Unit }) =
    closeable.close()

  type RN = Record { val name: String }

  def g(r: RN) = r.name

  val rr: RN = Record("name" -> "Bob", "age" -> 42).asInstanceOf[RN]

  def main(args: Array[String]): Unit = {
    f(new java.io.PrintStream("foo"))
    assert(g(rr) == "Bob")

    val s: { def concat(s: String): String } = "abc"
    assert(s.concat("def") == "abcdef")
  }
}

