// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit = {
    // these should give unboxed results
    println(().getClass)
    println(().getClass[Unit])
    println(5.getClass)
    println(5.getClass[Int])
    // these should give boxed results
    println(().asInstanceOf[AnyRef & Unit].getClass)
    println(().asInstanceOf[AnyRef & Unit].getClass[Any])
    println(().asInstanceOf[Unit & AnyRef].getClass)
    println(().asInstanceOf[Unit & AnyRef].getClass[Any])
    println(5.asInstanceOf[AnyRef & Int].getClass)
    println(5.asInstanceOf[AnyRef & Int].getClass[Any])
    println(5.asInstanceOf[Int & AnyRef].getClass)
    println(5.asInstanceOf[Int & AnyRef].getClass[Any])
    //make sure ## wasn't broken
    println(5.##)
    println((5.asInstanceOf[AnyRef]).##)
    println((5:Any).##)
  }
}
