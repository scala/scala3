object MyOtherPhantom extends Phantom {
  type MyPhantom[V] <: this.Any
  def myPhantom[X]: MyPhantom[X] = assume

  def f1[U, T <: MyPhantom[U]](a: Int, b: T,  c: Int): T = b

  def f2 = {
    f1(3, myPhantom[Int], 2)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f1 = MyOtherPhantom.getClass.getMethods.find(_.getName.endsWith("f1")).get
    val tParams = f1.getTypeParameters
    println(f1.toGenericString)
    tParams.foreach { tp =>
      println(tp.getName + " <: " + tp.getBounds.map(_.getTypeName).mkString(", "))
    }
  }
}
