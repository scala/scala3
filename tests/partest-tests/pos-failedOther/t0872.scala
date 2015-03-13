object Main {
  def main(args : Array[String]): Unit = {
    val fn = (a : Int, str : String) => "a: " + a + ", str: " + str
    implicit def fx[T](f : (T,String) => String): T => String = (x:T) => f(x,null)
    println(fn(1))
    ()
  }
}
