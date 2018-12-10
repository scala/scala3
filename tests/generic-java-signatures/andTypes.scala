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
      val tp1 = tp.nn
      println(tp1.getName + " <: " + tp1.getBounds.map(_.nn.getTypeName).mkString(", "))
    }
  }
}
