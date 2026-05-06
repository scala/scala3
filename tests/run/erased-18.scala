//> using options -language:experimental.erasedDefinitions

object Test {

  def main(args: Array[String]): Unit = {
    (
      (erased x: Int) => {
       println("lambda")
       42
     }
    )(foo)
  }

  inline def foo = {
    //println("foo")
    42
  }
}
