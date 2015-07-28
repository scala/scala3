trait A{
  private var s = 1
  def getS = s
}

object Test extends A {
 def main(args: Array[String]): Unit = println(getS)
}
