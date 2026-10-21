trait F[G[_]]

class A1(val a: F[List])
class A2()(using val a: F[List])

object Test:
  def main(args: Array[String]): Unit =
    List(classOf[A1], classOf[A2]).foreach(c =>
      c.getConstructors.foreach(m =>
        println(m.toGenericString)
      )
      c.getMethods.filter(_.getName == "a").foreach(m =>
        println(m.toGenericString)
      )
      println("---")
    )
