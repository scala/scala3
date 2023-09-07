package example

object Main {
  def main(args: Array[String]): Unit = {
    val x = A.f(new String(Array.empty[Byte]))
    println(x)
  }
}
