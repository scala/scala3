object Test:
  def main(args: Array[String]): Unit =
    (new A).m

class A:
  private lazy val x = 1
  def m: Int =
    lazy val y = 2
    x + y + A.z

object A:
  private lazy val z = 3
