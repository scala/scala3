trait T{
 def foo(a: List[String]): String = "1"
 def foo(a: List[Int]): Int = 1
 foo(List("1"))
 foo(List(1))
}
// to be compiled by dotty
object Test extends T{
  def main(args: Array[String]): Unit = ()
}
