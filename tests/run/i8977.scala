object Test {
  val pi = 3.1415926

  def main(args: Array[String]): Unit = {
    System.out.printf("pi = %6.4f\n", pi)
    System.out.printf("pi = %6.4f\n", Seq[scala.Double](pi):_*)
    System.out.printf("pi = %6.4f\n", Seq[java.lang.Double](pi):_*)
    System.out.printf("pi = %6.4f\n", Array[scala.Double](pi):_*)
    System.out.printf("pi = %6.4f\n", Array[java.lang.Double](pi):_*)
  }
}
