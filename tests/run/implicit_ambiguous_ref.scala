object A {
  val x: Int = 1
}

abstract class LowPriorityB {
  implicit val y: String = "y"
}

object B extends LowPriorityB {
  implicit val x: String = "x"
}

object Test {
  import A._, B._
  def main(args: Array[String]): Unit = {
    // B.x is an ambiguous ref, so LowPriorityB.y should be used
    println(implicitly[String])
  }
}
