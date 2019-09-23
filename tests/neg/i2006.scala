object Test {

  inline def foo(f: (given Int) => Int): AnyRef = f // error
  inline def bar(f: (given Int) => Int) = f // error

  def main(args: Array[String]) = {
    foo((given thisTransaction) => 43)
    bar((given thisTransaction) => 44)
  }
}
