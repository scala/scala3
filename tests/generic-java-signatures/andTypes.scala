class C1
trait T1
trait T2

class Foo {
  def foo[T <: (C1 & T1) & T2] = ()
}

object Test {
  def main(args: Array[String]): Unit = {
    val tParams = classOf[Foo].getDeclaredMethod("foo").getTypeParameters
    tParams.foreach { tp =>
      println(tp.getName + " <: " + tp.getBounds.map(_.getTypeName).mkString(", "))
    }
  }
}
