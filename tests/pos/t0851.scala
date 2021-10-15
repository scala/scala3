package test

object test1 {
  case class Foo[T,T2](f : (T,T2) => String) {
    def apply(t : T) = (s:T2) => f(t,s)
  }
  implicit def g[T](f : (T,String) => String): Foo[T, String] = Foo(f)
  def main(args : Array[String]) : Unit = {
    val f = (x:Int,s:String) => s + x
    println(f(1))
    ()
  }
}
