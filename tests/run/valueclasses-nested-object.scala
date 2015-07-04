class M(val t: Int) extends AnyVal {
  // Disallowed in Scala 2.11 (see SI-6359)
  def o = {
     object X {
       override def toString = t.toString
     }
     () => X
   }
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = new M(3)
    println(x.o())
  }
}
