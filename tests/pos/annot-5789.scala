class Annot[T] extends scala.annotation.Annotation

class D[T](val f: Int@Annot[T])

object A{
  def main(a:Array[String]) = {
    val c = new D[Int](1)
    c.f
  }
}
