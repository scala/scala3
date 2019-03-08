object Test {
  def main(args: Array[String]): Unit = {
    val a: Int = synchronized(1)
    val b: Long = synchronized(1L)
    val c: Boolean = synchronized(true)
    val d: Float = synchronized(1f)
    val e: Double = synchronized(1.0)
    val f: Byte = synchronized(1.toByte)
    val g: Char = synchronized('1')
    val h: Short = synchronized(1.toShort)
    val i: String = synchronized("Hello")
    val j: List[Int] = synchronized(List(1))
    synchronized(())
  }
}
