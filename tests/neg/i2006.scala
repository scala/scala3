object Test {

  inline def foo(f: ImplicitFunction1[Int, Int]): AnyRef = f // error
  inline def bar(f: ImplicitFunction1[Int, Int]) = f // error

  def main(args: Array[String]) = {
    foo(given thisTransaction => 43)
    bar(given thisTransaction => 44)
  }
}
