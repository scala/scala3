object O:
  object I:
    object CCC
  export I.*

  val foo_good: I.CCC.type = CCC
  val foo_bad: CCC.type = CCC

object Test:
  def main(args: Array[String]): Unit =
    classOf[O.type].getMethods.sortBy(_.getName).filter(_.getName.contains("foo")).foreach(m => {
      println(m)
      println(m.toGenericString)
    })

