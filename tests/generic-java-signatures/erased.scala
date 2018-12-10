object MyErased {
  def f1[U](erased a: Int): Int = 0
}

object Test {
  def main(args: Array[String]): Unit = {
    val f1 = MyErased.getClass.getMethods.find(_.nn.getName.endsWith("f1")).get.nn
    val tParams = f1.getTypeParameters
    println(f1.toGenericString)
    tParams.foreach { tp =>
      val tp1 = tp.nn
      println(tp1.getName + " <: " + tp1.getBounds.map(_.nn.getTypeName).mkString(", "))
    }
  }
}
